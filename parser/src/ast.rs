use location::Located;

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
    FloatLiteral(f64),
    StringLiteral(&'a str),
    RegexLiteral(&'a str),
    List(Vec<Located<'a, AST<'a>>>),
    Vector(Vec<Located<'a, AST<'a>>>),
    Set(Vec<Located<'a, AST<'a>>>),
    Map(Vec<(Located<'a, AST<'a>>, Located<'a, AST<'a>>)>),
    Symbol(Symbol<'a>),
    Keyword(Keyword<'a>),
    Quoted(Box<Located<'a, AST<'a>>>),
    SyntaxQuoted(Box<Located<'a, AST<'a>>>),
    Metadata(Vec<Located<'a, AST<'a>>>),
    Root(Vec<Located<'a, AST<'a>>>)
}
