use std::collections::HashMap;

use location::Location;
use semantic_parser::semantic_ast::{Source, Type, Function, TopLevel, Define};
pub enum ResolvedType {
    Int,
    Str,
    Unknown,
}

pub struct AnalyzeContext {
    pub variable_scopes: Vec<HashMap<String, Type>>,
    pub type_scopes: Vec<HashMap<Type, ResolvedType>>,
    pub aliases: HashMap<String, String>,
}

impl AnalyzeContext {
    pub fn new() -> Self {
        AnalyzeContext {
            variable_scopes: Vec::new(),
            type_scopes: Vec::new(),
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
    pub fn find_variable_type(&self, name: &str) -> Option<&Type> {
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
    loc: Location,
    message: String,
}

type Errors<'a> = &'a mut Vec<AnalyzeError>;

pub fn analyze_def(errors: Errors, def: Define) {

}

pub fn analyze_function(errors: Errors, func: Function) {

}

pub fn analyze_source(source: Source) {
    let mut errors = Vec::new();
    for top in source.toplevels {
        match top.value {
            TopLevel::NamespaceDef(_) => todo!(),
            TopLevel::Function(func) => analyze_function(&mut errors, func),
            TopLevel::Method(_) => todo!(),
            TopLevel::Def(def) => analyze_def(&mut errors, def),
            TopLevel::Unknown => todo!(),
        }
    }
}
