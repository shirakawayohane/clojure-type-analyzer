mod ast;

use std::{
    iter::{Enumerate, Map},
    slice::Iter,
};

pub use ast::AST;
use lexer::Token;
use location::Located;
use token_combinator::{TokenParseError, TokenParseResult};

type Tokens<'a> = &'a [Located<'a, Token<'a>>];

type Output<'a> = Located<'a, AST<'a>>;

type ParseResult<'a> = TokenParseResult<'a, Token<'a>, AST<'a>, Located<'a, Token<'a>>>;

pub mod nom {
    //! nom's result types, re-exported.
    pub use nom::{error::Error, error::ErrorKind, Err, IResult, Needed};
}

fn parse_symbol(tokens: Tokens) -> ParseResult {
    let (rest, _) = Token::l_paren(tokens)?;
    todo!();
    // let (rest, token) = token(TokenKind::Symbol)(tokens)?;
    // if let Token::Symbol(s) = token.into() {
    //     let splited = s.split('/').collect::<Vec<_>>();
    //     if splited.len() == 1 {
    //         let name = splited[0];
    //         return Ok((rest, AST::Symbol(ast::Symbol { ns: None, name })));
    //     } else if splited.len() == 2 {
    //         let ns = splited[0];
    //         let name = splited[1];
    //         return Ok((rest, AST::Symbol(ast::Symbol { name, ns: Some(ns) })));
    //     } else {
    //         unreachable!()
    //     }
    // } else {
    //     unreachable!()
    // }
    // if let Token::Symbol(s) = tokens[0].value {
    //     let splited = s.split('/').collect::<Vec<_>>();
    //     if splited.len() == 1 {
    //         let name = splited[0];
    //         Ok(AST::Symbol(Symbol { ns: None, name }))
    //     } else if splited.len() == 2 {
    //         let ns = splited[0];
    //         let name = splited[1];
    //         Ok(AST::Symbol(Symbol { name, ns: Some(ns) }))
    //     } else {
    //         unreachable!()
    //     }
    // }
}
