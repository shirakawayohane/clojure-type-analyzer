mod ast;

use std::{
    iter::{Enumerate, Map},
    ops::Deref,
    slice::Iter,
};

use ast::Symbol;
pub use ast::AST;
use lexer::Token;
use location::{Located, Span};
use token_combinator::{delimited, TokenParseError, TokenParseResult, TokenParser};

type Tokens<'a> = &'a [Located<'a, Token<'a>>];

type Output<'a> = Located<'a, AST<'a>>;

type ParseResult<'a> = TokenParseResult<'a, Token<'a>, AST<'a>, Located<'a, Token<'a>>>;

use lexer::token::parser::*;

fn parse_symbol(tokens: Tokens) -> ParseResult {
    let (rest, name) = symbol(tokens)?;
    let splited = name.split('/').collect::<Vec<_>>();
    if splited.len() == 1 {
        let name = splited[0];
        return Ok((rest, AST::Symbol(ast::Symbol { ns: None, name })));
    } else if splited.len() == 2 {
        let ns = splited[0];
        let name = splited[1];
        return Ok((rest, AST::Symbol(ast::Symbol { name, ns: Some(ns) })));
    } else {
        unreachable!()
    }
}

fn parse_keyword(tokens: Tokens) -> ParseResult {
    let (rest, name) = keyword(tokens)?;
    let splited = name.split('/').collect::<Vec<_>>();
    if splited.len() == 1 {
        let name = if splited[0].starts_with("::") {
            &name[2..]
        } else {
            &name[1..]
        };
        return Ok((rest, AST::Symbol(ast::Symbol { ns: None, name })));
    } else if splited.len() == 2 {
        let ns = if splited[0].starts_with("::") {
            &name[2..]
        } else {
            &name[1..]
        };
        let name = splited[1];
        return Ok((rest, AST::Symbol(ast::Symbol { name, ns: Some(ns) })));
    } else {
        unreachable!()
    }
}

fn apply_parser<'a, T, O, W, P>(tokens: &'a [W], mut parser: P) -> TokenParseResult<'a, T, O, W>
where
    T: Copy,
    W: 'a + Copy + Into<T>,
    P: TokenParser<'a, T, O, W>,
{
    parser.parse(tokens)
}

fn pair<'a, T, O1, O2, W, P1, P2>(
    tokens: &'a [W],
    mut first: P1,
    mut second: P2,
) -> TokenParseResult<'a, T, (O1, O2), W>
where
    T: Copy,
    W: 'a + Copy + Into<T>,
    P1: TokenParser<'a, T, O1, W>,
    P2: TokenParser<'a, T, O2, W>,
{
    let (tokens, first_result) = first.parse(tokens)?;
    let (tokens, second_result) = second.parse(tokens)?;
    Ok((tokens, (first_result, second_result)))
}

#[test]
fn test() {
    let tokens = vec![
        Located {
            span: Span::new("hogehoge"),
            value: Token::LParen,
        },
        Located {
            span: Span::new("hogehoge"),
            value: Token::RParen,
        },
    ];
    let (tokens, _) = pair(&tokens, l_paren, r_paren).unwrap();
    dbg!(tokens);
}
