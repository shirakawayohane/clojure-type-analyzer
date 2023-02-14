use std::collections::{HashMap};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub ns: Option<String>,
    pub name: String,
}

impl<'a> From<&parser::ast::Symbol<'a>> for Symbol {
    fn from(value: &parser::ast::Symbol<'a>) -> Self {
        Self {
            ns: value.ns.map(|x| x.to_string()),
            name: value.name.to_string()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub simple: Option<String>,
    pub keys: Option<Vec<String>>,
    pub alias: Option<Vec<(String, String)>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub cond: Box<Expression>,
    pub when_true: Box<Expression>,
    pub when_false: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhenExpression {
    pub cond: Box<Expression>,
    pub when_true: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PredicateValueSet {
    pub predicate: Box<Expression>,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CondExpression(Vec<PredicateValueSet>);

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub func_expr: Box<Expression>,
    pub args: Vec<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseExpression {
    pub cases: Vec<PredicateValueSet>,
    pub default: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    SymbolRef(Symbol),
    Call(CallExpression),
    When(WhenExpression),
    If(IfExpression),
    Cond(CondExpression),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type {
    Int,
    Any
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub arguments: Vec<(Binding, Type)>,
    pub return_type: Type
}

#[derive(Debug, PartialEq, Clone)]
pub struct Define {
    pub name: String,
    pub ty: Type,
    pub value: Expression
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub decl: FunctionDecl,
    pub exprs: Vec<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct RequireDef {
    pub aliases: HashMap<Symbol, Symbol>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamespaceDef {
    pub namespace: String,
    pub require: RequireDef
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevel {
    NamespaceDef(NamespaceDef),
    Function(FunctionDecl),
    Def(Define),
    Unknown
}

#[derive(Debug, PartialEq)]
pub struct Root {
    pub toplevels: Vec<TopLevel>
}
