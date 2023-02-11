#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Symbol {
    ns: Option<String>,
    name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding {
    simple: Option<String>,
    keys: Option<Vec<String>>,
    alias: Option<Vec<(String, String)>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    cond: Box<Expression>,
    when_true: Box<Expression>,
    when_false: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhenExpression {
    cond: Box<Expression>,
    when_true: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PredicateValueSet {
    predicate: Expression,
    value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CondExpression(Vec<PredicateValueSet>);

#[derive(Debug, Clone, PartialEq)]
pub struct CaseExpression {
    cases: Vec<PredicateValueSet>,
    defualt: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    IntegerLiteral(i64),
    NumberLiteral(f64),
    StringLiteral(String),
    Vector(Vec<Expression>),
    Set(Vec<Expression>),
    Map(Vec<(Expression, Expression)>),
    SymbolRef(Symbol),
    Call(Box<Expression>, Vec<Expression>),
    LetBinding(LetBinding, Vec<Expression>),
    For(LetBinding),
    // cond        when true   when false
    If(IfExpression),
}
