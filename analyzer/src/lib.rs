use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    fmt::{Display, Write},
    rc::Rc,
};

use derive_is_enum_variant::is_enum_variant;
use location::Location;
use semantic_parser::semantic_ast::{
    DefSchema, Define, Expression, Function, FunctionDecl, MapKey, Source, TopLevel, Type,
};

mod expression;
use expression::*;

#[derive(Debug, Clone, PartialEq, Eq, is_enum_variant)]
pub enum ResolvedType {
    Nil,
    Int,
    Num,
    Str,
    Keyword,
    Map(HashMap<MapKey, ResolvedType>),
    Array(Box<ResolvedType>),
    Fn {
        return_ty: Box<ResolvedType>,
        arg_types: Vec<ResolvedType>,
    },
    Class(String),
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
            ResolvedType::Array(inner_type) => write!(f, "[{}]", inner_type),
            ResolvedType::Map(kvp) => {
                f.write_char('{')?;
                for (k, v) in kvp.iter() {
                    write!(f, "{} {}, ", k, v)?;
                }
                f.write_char('}')?;
                Ok(())
            }
            ResolvedType::Class(name) => f.write_str(&name),
        }
    }
}

impl ResolvedType {
    pub fn is_assignable_to(&self, other: &Self, context: Context) -> bool {
        if other.is_unknown() {
            return true;
        }
        match self {
            ResolvedType::Nil => other.is_nil(),
            ResolvedType::Int => other.is_int() || other.is_num(),
            ResolvedType::Str => other.is_str(),
            ResolvedType::Num => other.is_num(),
            ResolvedType::Keyword => other.is_keyword(),
            ResolvedType::Array(inner_type) => {
                if let ResolvedType::Array(other_inner_type) = *inner_type.clone() {
                    inner_type.is_assignable_to(&other_inner_type, context)
                } else {
                    false
                }
            }
            ResolvedType::Map(kvs) => {
                if let ResolvedType::Map(other_kvs) = other {
                    dbg!(other_kvs);
                    if other_kvs.len() == 1 {
                        if let MapKey::Type(other_key_ty) = other_kvs.keys().next().unwrap() {
                            let resolved_other_key_ty = context.borrow().resolve_type(other_key_ty);
                            let resolved_other_value_ty = other_kvs.values().next().unwrap();
                            for (k, resolved_v_ty) in kvs.iter() {
                                let resolved_k_ty = match k {
                                    MapKey::Type(t) => context.borrow().resolve_type(t),
                                    MapKey::Keyword(_) => ResolvedType::Keyword,
                                    MapKey::String(_) => ResolvedType::Str,
                                    MapKey::Integer(_) => ResolvedType::Int,
                                    MapKey::Unknown => ResolvedType::Unknown,
                                };
                                if !resolved_k_ty
                                    .is_assignable_to(&resolved_other_key_ty, context.clone())
                                    || !resolved_v_ty
                                        .is_assignable_to(resolved_other_value_ty, context.clone())
                                {
                                    return false;
                                }
                            }
                            return true;
                        }
                    }

                    for (other_k, other_v_ty) in other_kvs {
                        if let Some(self_v) = kvs.get(other_k) {
                            if self_v.is_assignable_to(other_v_ty, context.clone()) {
                                continue;
                            }
                        }
                        return false;
                    }
                    return true;
                }
                return false;
            }
            ResolvedType::Unknown => true,
            ResolvedType::Fn {
                return_ty: _,
                arg_types: _,
            } => other.is_fn(), // TODO https://github.com/WiZLite/clojure-parser-rs/issues/6
            ResolvedType::Class(name) => {
                if let ResolvedType::Class(other_name) = other {
                    name == other_name
                } else {
                    false
                }
            }
        }
    }
}

pub struct AnalyzeContext {
    pub variable_scopes: Vec<HashMap<String, ResolvedType>>,
    pub type_scopes: Vec<HashMap<String, ResolvedType>>,
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
        global_type_scope.insert("Int".to_string(), ResolvedType::Int);
        global_type_scope.insert("Num".to_string(), ResolvedType::Num);
        global_type_scope.insert("Str".to_string(), ResolvedType::Str);
        global_type_scope.insert("Keyword".to_string(), ResolvedType::Keyword);

        AnalyzeContext {
            variable_scopes: vec![global_variable_scope],
            type_scopes: vec![global_type_scope],
            aliases: HashMap::new(),
        }
    }
    pub fn resolve_type(&self, ty: &Type) -> ResolvedType {
        for types_in_scope in self.type_scopes.iter().rev() {
            match ty {
                Type::Scalar(name) => {
                    if let Some(ty) = types_in_scope.get(name) {
                        return ty.clone();
                    } else {
                        return ResolvedType::Unknown;
                    }
                }
                Type::Array(inner_type) => {
                    return ResolvedType::Array(Box::new(self.resolve_type(inner_type)))
                }
                Type::Map(kvs) => {
                    return ResolvedType::Map(
                        kvs.into_iter()
                            .map(|(k, v)| (k.clone(), self.resolve_type(v)))
                            .collect::<HashMap<_, _>>(),
                    )
                }
                Type::Any => todo!(),
                Type::Unknown => todo!(),
            }
        }
        ResolvedType::Unknown
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
    pub loc: (Location, Location),
    pub message: String,
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
        Expression::StringLiteral(_) => ResolvedType::Str,
        Expression::RegexLiteral(_) => todo!(),
        Expression::Keyword(_) => ResolvedType::Keyword,
        Expression::SymbolRef(sym) => context
            .borrow()
            .find_variable_type(sym.name)
            .unwrap_or(&ResolvedType::Unknown)
            .clone(),
        Expression::SetLiteral(_) => todo!(),
        Expression::VectorLiteral(_) => todo!(),
        Expression::MapLiteral(kvs) => ResolvedType::Map(
            kvs.into_iter()
                .map(|(k, v)| {
                    let map_key = match &k.value {
                        Expression::IntLiteral(i) => MapKey::Integer(*i),
                        Expression::FloatLiteral(_) => MapKey::Type(Type::Scalar("Num".to_owned())),
                        Expression::StringLiteral(s) => MapKey::String(s.to_owned()),
                        Expression::RegexLiteral(_) => {
                            MapKey::Type(Type::Scalar("java.util.regex.Pattern".to_owned()))
                        }
                        Expression::Keyword(k) => MapKey::Keyword(k.fullname()),
                        _ => MapKey::Unknown,
                    };
                    let v_ty = infer_expression_type(errors, context.clone(), v);
                    (map_key, v_ty)
                })
                .collect(),
        ),
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
        Expression::AnonymousFn(exprs) => {
            if exprs.len() == 0 {
                ResolvedType::Nil
            } else {
                if let ResolvedType::Fn { return_ty, arg_types: _ } = infer_expression_type(errors, context, exprs.first().unwrap()) {
                    *return_ty
                } else {
                    ResolvedType::Unknown
                }
            }
        },
    }
}

pub fn analyze_def(errors: Errors, context: Context, def: &Define) {
    let value_ty = infer_expression_type(errors, context.clone(), &def.value);
    if let Some(ty) = &def.ty {
        let resolved = context.borrow().resolve_type(&ty);
        if !value_ty.is_assignable_to(&resolved, context.clone()) {
            errors.push(AnalyzeError {
                loc: def.value.range,
                message: format!("{} is not assinable to {}", value_ty, resolved),
            })
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
        context.borrow().resolve_type(ty).clone()
    } else {
        ResolvedType::Unknown
    };
    let arg_types = decl
        .arguments
        .iter()
        .map(|arg| {
            if let Some(arg_ty) = &arg.ty_annotation {
                context.borrow_mut().resolve_type(&arg_ty.value).clone()
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
    let func_ty = get_func_type(context.clone(), &func.decl);
    context
        .borrow_mut()
        .variable_scopes
        .last_mut()
        .unwrap()
        .insert(func.decl.name.clone(), func_ty);
    variable_scope!(context, {
        for arg in &func.decl.arguments {

            match &arg.binding.value {
                semantic_parser::semantic_ast::Binding::Simple(name) => {
                    let arg_ty = if let Some(arg_ty) = &arg.ty_annotation {
                        context.borrow().resolve_type(&arg_ty).clone()
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

pub fn analyze_schema(_errors: Errors, context: Context, schema: &DefSchema) {
    let resolved = context.borrow().resolve_type(&schema.ty);
    context
        .borrow_mut()
        .type_scopes
        .last_mut()
        .unwrap()
        .insert(schema.name.to_owned(), resolved);
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
            TopLevel::DefSchema(schema) => analyze_schema(&mut errors, context.clone(), &schema),
        }
    }
    errors
}
