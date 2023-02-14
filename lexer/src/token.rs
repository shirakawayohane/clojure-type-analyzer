use location::{Located, Span};
use token_combinator::TokenParser;

#[derive(Debug, PartialEq, Clone, Copy, TokenParser)]
pub enum Token<'a> {
    LParen,      // (
    RParen,      // )
    LBracket,    // [
    RBracket,    // ]
    LBrace,      // {
    RBrace,      // }
    Quote,       // '
    SyntaxQuote, // `
    Hat,         // ^
    Sharp,       // #
    StringLiteral(Span<'a>),
    IntegerLiteral(i64), // 10, 0xFF, 0b01, 0o70...
    FloatLiteral(f64), // 3.14...
    Keyword(Span<'a>), // :keyword, ::keyword, ::ns/keyword :key.word ...
    Symbol(Span<'a>),  // symbol, ns/symbol ...
}

impl<'a> From<Located<'a, Token<'a>>> for Token<'a> {
    fn from(value: Located<'a, Token<'a>>) -> Token<'a> {
        value.value
    }
}