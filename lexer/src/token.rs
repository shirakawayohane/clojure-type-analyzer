use std::fmt::{Display, Write};

use location::{Located, Span};
use token_combinator::{GetKind, ParseToken};

#[derive(Debug, PartialEq, Clone, Copy, ParseToken)]
pub enum Token<'a> {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Quote,
    SyntaxQuote,
    Hat,
    Sharp,
    StringLiteral(Span<'a>),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    Keyword(Span<'a>),
    Symbol(Span<'a>),
}
