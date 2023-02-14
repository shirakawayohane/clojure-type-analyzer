pub mod ast;

pub use ast::AST;
use lexer::Token;
use location::{Located, Span};
use token_combinator::{alt, delimited, many0, many1, map, preceded, tuple, TokenParseResult, TokenParser};

type Tokens<'a> = &'a [Located<'a, Token<'a>>];

type ParseResult<'a> = TokenParseResult<'a, Token<'a>, Located<'a, AST<'a>>, Located<'a, Token<'a>>>;

use lexer::token::parser::*;

fn located<'a>(
    mut parser: impl TokenParser<'a, Token<'a>, AST<'a>, Located<'a, Token<'a>>>,
) -> impl FnMut(&'a [Located<'a, Token<'a>>]) -> TokenParseResult<'a, Token<'a>, Located<'a, AST<'a>>, Located<'a, Token<'a>>> {
    move |tokens: &'a [Located<'a, Token<'a>>]| {
        let from = tokens[0].span;
        let (rest, output) = parser.parse(tokens)?;
        let to = rest[0].span;
        let span = unsafe {
            let from_offset = from.location_offset();
            let to_offset = to.location_offset();
            let fragment = &from.fragment()[0..(to_offset - from_offset)];
            Span::new_from_raw_offset(from.location_offset(), from.location_line(), fragment, ())
        };
        Ok((
            rest,
            Located {
                span,
                value: output,
            },
        ))
    }
}


fn parse_symbol(tokens: Tokens) -> ParseResult {
    located(map(symbol, |symbol_str| {
        let splited = symbol_str.split('/').collect::<Vec<_>>();
        if splited.len() == 1 {
            let name = splited[0];
            return AST::Symbol(ast::Symbol { ns: None, name });
        } else if splited.len() == 2 {
            let ns = splited[0];
            let name = splited[1];
            return AST::Symbol(ast::Symbol { name, ns: Some(ns) });
        } else {
            unreachable!()
        }
    }))(tokens)
}

fn parse_keyword(tokens: Tokens) -> ParseResult {
    located(map(keyword, |keyword_str| {
        let name = if keyword_str.starts_with("::") {
            &keyword_str[2..]
        } else {
            &keyword_str[1..]
        };
        let splited = name.split('/').collect::<Vec<_>>();
        if splited.len() == 1 {
            let name = splited[0];
            return AST::Keyword(ast::Keyword { ns: None, name });
        } else if splited.len() == 2 {
            let ns = splited[0];
            let name = splited[1];
            return AST::Keyword(ast::Keyword { name, ns: Some(ns) });
        } else {
            unreachable!()
        }
    }))(tokens)
}

fn parse_string_literal(tokens: Tokens) -> ParseResult {
    located(map(string_literal, |str| AST::StringLiteral(&str)))(tokens)
}

fn parse_integer_literal(tokens: Tokens) -> ParseResult {
    located(map(integer_literal, |i| AST::IntegerLiteral(i)))(tokens)
}

fn parse_float_literal(tokens: Tokens) -> ParseResult {
    located(map(float_literal, |f| AST::FloatLiteral(f)))(tokens)
}

fn parse_list(tokens: Tokens) -> ParseResult {
    located(map(delimited(l_paren, many0(parse_form), r_paren), |forms| {
        AST::List(forms)
    }))(tokens)
}

fn parse_vector(tokens: Tokens) -> ParseResult {
    located(map(
        delimited(l_bracket, many0(parse_form), r_bracket),
        |forms| AST::Vector(forms),
    ))(tokens)
}

fn parse_map(tokens: Tokens) -> ParseResult {
    located(map(
        delimited(l_brace, many0(tuple((parse_form, parse_form))), r_brace),
        |kvs| AST::Map(kvs),
    ))(tokens)
}

fn parse_set(tokens: Tokens) -> ParseResult {
    located(map(
        tuple((sharp, delimited(l_brace, many0(parse_form), r_brace))),
        |(_, forms)| AST::Set(forms),
    ))(tokens)
}

fn parse_regex_literal(tokens: Tokens) -> ParseResult {
    located(map(preceded(sharp, string_literal), |str| {
        AST::RegexLiteral(&str)
    }))(tokens)
}

fn parse_quoted_form(tokens: Tokens) -> ParseResult {
    located(map(preceded(quote, parse_form), |form| {
        AST::Quoted(Box::new(form))
    }))(tokens)
}

fn parse_syntax_quoted_form(tokens: Tokens) -> ParseResult {
    located(map(preceded(syntax_quote, parse_form), |form| {
        AST::SyntaxQuoted(Box::new(form))
    }))(tokens)
}

fn parse_metadata(tokens: Tokens) -> ParseResult {
    located(map(
        many1(preceded(
            hat,
            parse_form
        )),
        |meta_forms| AST::Metadata(meta_forms),
    ))(tokens)
}

pub fn parse_form(tokens: Tokens) -> ParseResult {
    alt((
        parse_symbol,
        parse_keyword,
        parse_string_literal,
        parse_integer_literal,
        parse_float_literal,
        parse_list,
        parse_vector,
        parse_map,
        parse_set,
        parse_regex_literal,
        parse_quoted_form,
        parse_syntax_quoted_form,
        parse_metadata
    ))(tokens)
}

pub fn parse_root(tokens: Tokens) -> ParseResult {
    located(map(many1(parse_form), |top_forms| {
        AST::Root(top_forms)
    }))(tokens)
}