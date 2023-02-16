use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    fmt::Display,
    rc::Rc,
};

use derive_is_enum_variant::is_enum_variant;
use location::Location;
use semantic_parser::semantic_ast::{
    Define, Expression, Function, FunctionDecl, Source, TopLevel, Type,
};

mod expression;
use expression::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, is_enum_variant)]
pub enum ResolvedType {
    Nil,
    Int,
    Num,
    Str,
    Keyword,
    Fn {
        return_ty: Box<ResolvedType>,
        arg_types: Vec<ResolvedType>,
    },
    Unknown,
}

impl Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Nil => write!(f, "Nil"),
            ResolvedType::Int => write!(f, "Int"),
            ResolvedType::Num => write!(f, "Num"),
            ResolvedType::Str => write!(f, "Str"),
            ResolvedType::Keyword => write!(f, "Keyword"),
            ResolvedType::Unknown => write!(f, "Unknown"),
            ResolvedType::Fn {
                return_ty: _,
                arg_types: _,
            } => write!(f, "Function"),
        }
    }
}

impl ResolvedType {
    pub fn is_assinable_to(&self, other: &Self) -> bool {
        if other.is_unknown() {
            return true;
        }
        match self {
            ResolvedType::Nil => other.is_nil(),
            ResolvedType::Int => other.is_int() || other.is_num(),
            ResolvedType::Str => other.is_str(),
            ResolvedType::Num => other.is_num(),
            ResolvedType::Keyword => other.is_keyword(),
            ResolvedType::Unknown => true,
            ResolvedType::Fn {
                return_ty: _,
                arg_types: _,
            } => other.is_fn(), // TODO https://github.com/WiZLite/clojure-parser-rs/issues/6
        }
    }
}

pub struct AnalyzeContext {
    pub variable_scopes: Vec<HashMap<String, ResolvedType>>,
    pub type_scopes: Vec<HashMap<Type, ResolvedType>>,
    pub aliases: HashMap<String, String>,
}

impl AnalyzeContext {
    pub fn new() -> Self {
        let mut global_variable_scope = HashMap::new();
        global_variable_scope.insert(
            "+".into(),
            ResolvedType::Fn {
                return_ty: Box::new(ResolvedType::Num),
                arg_types: vec![ResolvedType::Num, ResolvedType::Num],
            },
        );

        let mut global_type_scope = HashMap::new();
        global_type_scope.insert(Type::Scalar("Int".to_string()), ResolvedType::Int);
        global_type_scope.insert(Type::Scalar("Num".to_string()), ResolvedType::Num);
        global_type_scope.insert(Type::Scalar("Str".to_string()), ResolvedType::Str);
        global_type_scope.insert(Type::Scalar("Keyword".to_string()), ResolvedType::Keyword);

        AnalyzeContext {
            variable_scopes: vec![global_variable_scope],
            type_scopes: vec![global_type_scope],
            aliases: HashMap::new(),
        }
    }
    pub fn resolve_type(&self, ty: &Type) -> Option<&ResolvedType> {
        for types_in_scope in self.type_scopes.iter().rev() {
            if let Some(v) = types_in_scope.get(ty) {
                return Some(v);
            }
        }
        None
    }
    pub fn find_variable_type<'a>(&'a self, name: &str) -> Option<&'a ResolvedType> {
        for scope in self.variable_scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalyzeError {
    loc: (Location, Location),
    message: String,
}

#[macro_export]
macro_rules! variable_scope {
    ($context: ident, $scope: block) => {
        $context.borrow_mut().variable_scopes.push(HashMap::new());
        $scope
        $context.borrow_mut().variable_scopes.pop();
    };
}

type Errors<'a> = &'a mut Vec<AnalyzeError>;

type Context<'a> = Rc<RefCell<AnalyzeContext>>;

pub fn infer_expression_type<'a>(
    errors: Errors,
    context: Context,
    expr: &'a Expression,
) -> ResolvedType {
    match expr {
        Expression::IntLiteral(_) => ResolvedType::Int,
        Expression::FloatLiteral(_) => ResolvedType::Num,
        Expression::Keyword(_) => ResolvedType::Keyword,
        Expression::SymbolRef(sym) => context
            .borrow()
            .find_variable_type(sym.name)
            .unwrap_or(&ResolvedType::Unknown)
            .clone(),
        Expression::SetLiteral(_) => todo!(),
        Expression::VectorLiteral(_) => todo!(),
        Expression::MapLiteral(_) => todo!(),
        Expression::Call(call_expr) => {
            if let ResolvedType::Fn {
                return_ty,
                arg_types: _,
            } = infer_expression_type(errors, context, &call_expr.func_expr)
            {
                *return_ty
            } else {
                ResolvedType::Unknown
            }
        }
        Expression::If(_) => todo!(),
        Expression::Cond(_) => todo!(),
        Expression::Let(let_expr) => {
            if let Some(last_expr) = let_expr.body.last() {
                infer_expression_type(errors, context, &last_expr)
            } else {
                ResolvedType::Nil
            }
        }
        Expression::Unknown => todo!(),
        Expression::When(_) => todo!(),
    }
}

pub fn analyze_def(errors: Errors, context: Context, def: &Define) {
    let value_ty = infer_expression_type(errors, context.clone(), &def.value);
    if let Some(ty) = &def.ty {
        if let Some(resolved) = context.borrow().resolve_type(&ty) {
            if !value_ty.is_assinable_to(resolved) {
                errors.push(AnalyzeError {
                    loc: def.value.range,
                    message: format!("{} is not assinable to {}", value_ty, resolved),
                })
            }
        }
    }
    context
        .borrow_mut()
        .variable_scopes
        .last_mut()
        .unwrap()
        .insert(def.name.to_string(), value_ty.clone());
}

pub fn get_func_type(context: Context, decl: &FunctionDecl) -> ResolvedType {
    let return_ty = if let Some(ty) = &decl.return_type {
        context
            .borrow()
            .resolve_type(ty)
            .unwrap_or(&ResolvedType::Unknown)
            .clone()
    } else {
        ResolvedType::Unknown
    };
    let arg_types = decl
        .arguments
        .iter()
        .map(|(_, opt_arg_ty)| {
            if let Some(arg_ty) = opt_arg_ty {
                context
                    .borrow_mut()
                    .resolve_type(arg_ty)
                    .unwrap_or(&ResolvedType::Unknown)
                    .clone()
            } else {
                ResolvedType::Unknown
            }
        })
        .collect::<Vec<_>>();

    ResolvedType::Fn {
        return_ty: Box::new(return_ty),
        arg_types,
    }
}

pub fn analyze_function(errors: Errors, context: Context, func: &Function) {
    println!("analyzing function");
    let func_ty = get_func_type(context.clone(), &func.decl);
    context
        .borrow_mut()
        .variable_scopes
        .last_mut()
        .unwrap()
        .insert(func.decl.name.clone(), func_ty);
    variable_scope!(context, {
        for (arg_binding, opt_arg_ty) in &func.decl.arguments {
            match &arg_binding.value {
                semantic_parser::semantic_ast::Binding::Simple(name) => {
                    let arg_ty = if let Some(arg_ty) = opt_arg_ty {
                        context
                            .borrow()
                            .resolve_type(&arg_ty)
                            .unwrap_or(&ResolvedType::Unknown)
                            .clone()
                    } else {
                        ResolvedType::Unknown
                    };
                    context
                        .borrow_mut()
                        .variable_scopes
                        .last_mut()
                        .unwrap()
                        .insert(name.clone(), arg_ty);
                }
                semantic_parser::semantic_ast::Binding::Complex { keys, alias } => todo!(),
            }
        }
        for expr in &func.exprs {
            analyze_expression(errors, context.clone(), expr);
        }
    });
}

pub fn analyze_source(source: Source) -> Vec<AnalyzeError> {
    let mut errors = Vec::new();
    let context = Rc::new(RefCell::new(AnalyzeContext::new()));
    for top in source.toplevels {
        match top.value {
            TopLevel::NamespaceDef(_) => todo!(),
            TopLevel::Function(func) => analyze_function(&mut errors, context.clone(), &func),
            TopLevel::Method(_) => todo!(),
            TopLevel::Def(def) => analyze_def(&mut errors, context.clone(), &def),
            TopLevel::Unknown => continue,
        }
    }
    errors
}
