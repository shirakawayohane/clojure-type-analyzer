use location::Located;
use token_combinator::TokenParser;

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol<'a> {
    pub ns: Option<&'a str>,
    pub name: &'a str,
}

pub struct Metadata<'a> {
    pub data: Vec<Located<AST<'a>>>,
}

impl Symbol<'_> {
    pub fn fullname(&self) -> String {
        if let Some(ns) = self.ns {
            format!("{}/{}", ns, self.name)
        } else {
            self.name.to_owned()
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Keyword<'a> {
    pub ns: Option<&'a str>,
    pub name: &'a str,
}

impl Keyword<'_> {
    pub fn fullname(&self) -> String {
        if let Some(ns) = self.ns {
            format!("::{}/{}", ns, self.name)
        } else {
            self.name.to_owned()
        }
    }
}

#[derive(Debug, Clone, PartialEq, TokenParser)]
pub enum AST<'a> {
    IntegerLiteral(i64),
    FloatLiteral(f64),
    CharLiteral(char),
    StringLiteral(&'a str),
    RegexLiteral(&'a str),
    AnonymousFn(Vec<Located<AST<'a>>>),
    List(Vec<Located<AST<'a>>>),
    Vector(Vec<Located<AST<'a>>>),
    Set(Vec<Located<AST<'a>>>),
    Map(Vec<Located<AST<'a>>>),
    AtomDeref(Symbol<'a>),
    Symbol(Symbol<'a>),
    And,
    Unquoted(Symbol<'a>),
    UnquotedSplicing(Symbol<'a>),
    Keyword(Keyword<'a>),
    Metadata(Box<Located<AST<'a>>>),
    Quoted(Box<Located<AST<'a>>>),
    SyntaxQuoted(Box<Located<AST<'a>>>),
    Root(Vec<Located<AST<'a>>>),
}

impl AST<'_> {
    // TODO: define in proc macro later
    pub fn integer_or_none(&self) -> Option<&i64> {
        if let AST::IntegerLiteral(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn keyword_or_none(&self) -> Option<&Keyword> {
        if let AST::Keyword(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn symbol_or_none(&self) -> Option<&Symbol> {
        if let AST::Symbol(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn vector_or_none(&self) -> Option<&Vec<Located<AST<'_>>>> {
        if let AST::Vector(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn list_or_none(&self) -> Option<&Vec<Located<AST<'_>>>> {
        if let AST::List(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
