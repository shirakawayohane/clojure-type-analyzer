#[derive(Debug, Clone, PartialEq)]
pub struct Symbol<'a> {
    pub ns: Option<&'a str>,
    pub name: &'a str,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Keyword<'a> {
    pub ns: Option<&'a str>,
    pub name: &'a str,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST<'a> {
    IntegerLiteral(i64),
    NumberLiteral(f64),
    StringLiteral(&'a str),
    List(Vec<AST<'a>>),
    Vector(Vec<AST<'a>>),
    Set(Vec<AST<'a>>),
    Map(Vec<(AST<'a>, AST<'a>)>),
    Symbol(Symbol<'a>),
    Keyword(Keyword<'a>),
    Quoted(Box<AST<'a>>),
}
