use std::collections::HashMap;

use location::Located;
use parser::{
    ast::{Keyword, Symbol},
    AST,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    Simple(String),
    Complex {
        keys: Option<Vec<String>>,
        alias: Option<(String, String)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression<'a> {
    pub cond: Box<Located<Expression<'a>>>,
    pub when_true: Box<Located<Expression<'a>>>,
    pub when_false: Option<Box<Located<Expression<'a>>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhenExpression<'a> {
    pub cond: Box<Located<Expression<'a>>>,
    pub when_true: Box<Located<Expression<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PredicateValueSet<'a> {
    pub predicate: Box<Expression<'a>>,
    pub value: Box<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CondExpression<'a>(Vec<PredicateValueSet<'a>>);

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression<'a> {
    pub func_expr: Box<Expression<'a>>,
    pub args: Vec<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseExpression<'a> {
    pub cases: Vec<PredicateValueSet<'a>>,
    pub default: Option<Box<Expression<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    IntLiteral(i64),
    FloatLiteral(f64),
    Keyword(&'a Keyword<'a>),
    SymbolRef(&'a Symbol<'a>),
    SetLiteral(Vec<Located<Expression<'a>>>),
    VectorLiteral(Vec<Located<Expression<'a>>>),
    MapLiteral(Vec<(Located<Expression<'a>>, Located<Expression<'a>>)>),
    Call(CallExpression<'a>),
    When(WhenExpression<'a>),
    If(IfExpression<'a>),
    Cond(CondExpression<'a>),
    Unknown,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Scalar(String),
    Array(Box<Type>),
    Any,
    Unknown,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub arguments: Vec<(Located<Binding>, Option<Located<Type>>)>,
    pub return_type: Option<Located<Type>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Define<'a> {
    pub name: &'a str,
    pub ty: Type,
    pub value: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function<'a> {
    pub decl: FunctionDecl,
    pub exprs: Vec<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RequireDef {
    pub aliases: HashMap<String, String>,
    // TODO: impl refer all
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamespaceDef {
    pub namespace: String,
    pub require: RequireDef,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevel<'a> {
    NamespaceDef(NamespaceDef),
    Function(FunctionDecl),
    Def(Define<'a>),
    Unknown,
}

#[derive(Debug, PartialEq)]
pub struct Root<'a> {
    pub toplevels: Vec<TopLevel<'a>>,
}
