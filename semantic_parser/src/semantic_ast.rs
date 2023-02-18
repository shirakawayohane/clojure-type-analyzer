use std::fmt::{Display, Write};

use location::Located;
use parser::ast::{Keyword, Symbol};

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
    pub func_expr: Box<Located<Expression<'a>>>,
    pub args: Vec<Located<Expression<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseExpression<'a> {
    pub cases: Vec<PredicateValueSet<'a>>,
    pub default: Option<Box<Expression<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetExpression<'a> {
    pub bindings: Vec<(
        Located<Binding>,
        Option<Located<Type>>,
        Located<Box<Expression<'a>>>,
    )>,
    pub body: Vec<Located<Expression<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    RegexLiteral(String),
    Keyword(&'a Keyword<'a>),
    SymbolRef(&'a Symbol<'a>),
    SetLiteral(Vec<Located<Expression<'a>>>),
    VectorLiteral(Vec<Located<Expression<'a>>>),
    MapLiteral(Vec<(Located<Expression<'a>>, Located<Expression<'a>>)>),
    Call(CallExpression<'a>),
    When(WhenExpression<'a>),
    If(IfExpression<'a>),
    Cond(CondExpression<'a>),
    Let(LetExpression<'a>),
    AnonymousFn(Vec<Located<Expression<'a>>>),
    Unknown,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum MapKey {
    Type(Type),
    Keyword(String),
    String(String),
    Integer(i64),
    Unknown,
}

impl Display for MapKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MapKey::Keyword(k) => k.fmt(f),
            MapKey::String(s) => s.fmt(f),
            MapKey::Integer(i) => i.fmt(f),
            MapKey::Unknown => write!(f, "Unknown"),
            MapKey::Type(ty) => ty.fmt(f),
        }
    }
}

impl Into<Type> for MapKey {
    fn into(self) -> Type {
        match self {
            MapKey::Type(t) => t,
            MapKey::Keyword(_) => Type::Scalar("Keyword".to_owned()),
            MapKey::String(_) => Type::Scalar("Str".to_owned()),
            MapKey::Integer(_) => Type::Scalar("Int".to_owned()),
            MapKey::Unknown => Type::Unknown,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Scalar(String),
    Array(Box<Type>),
    Map(Vec<(MapKey, Type)>),
    Any,
    Unknown,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Scalar(s) => s.fmt(f),
            Type::Array(t) => write!(f, "[{}]", t),
            Type::Map(kvs) => {
                f.write_char('{')?;
                for (k, v) in kvs {
                    writeln!(f, "{} {}", k, v)?;
                }
                f.write_char('}')?;
                Ok(())
            }
            Type::Any => f.write_str("Any"),
            Type::Unknown => f.write_str("Unknown"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Argument {
    pub binding: Located<Binding>,
    pub ty_annotation: Option<Located<Type>>,
    pub is_var_arg: bool
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub arguments: Vec<Located<Argument>>,
    pub return_type: Option<Located<Type>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Define<'a> {
    pub name: &'a str,
    pub ty: Option<Located<Type>>,
    pub value: Located<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DefSchema {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function<'a> {
    pub decl: FunctionDecl,
    pub exprs: Vec<Located<Expression<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Method {}

#[derive(Debug, Clone, PartialEq)]
pub struct Alias {
    pub ns: String,
    pub alias: String,
}
#[derive(Debug, Clone, PartialEq)]
pub struct NamespaceOnly {
    pub ns: String,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Refers {
    pub ns: String,
    pub refers: Vec<String>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ReferAll {
    pub ns: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RequireDef {
    pub aliases: Vec<Located<Alias>>,
    pub namespace_onlys: Vec<Located<NamespaceOnly>>,
    pub refers: Vec<Located<Refers>>,
    pub refer_alls: Vec<Located<ReferAll>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDef {
    // TODO: impl later
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamespaceDef {
    pub namespace: String,
    pub require: Option<Located<RequireDef>>,
    pub import: Option<Located<ImportDef>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevel<'a> {
    DefSchema(DefSchema),
    NamespaceDef(NamespaceDef),
    Function(Function<'a>),
    Method(Method),
    Def(Define<'a>),
    Unknown,
}

#[derive(Debug, PartialEq)]
pub struct Source<'a> {
    pub ns_def: Located<NamespaceDef>,
    pub toplevels: Vec<Located<TopLevel<'a>>>,
}
