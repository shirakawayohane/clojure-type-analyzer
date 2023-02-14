use token_combinator::tuple;
use token_combinator_macros::TokenParser;

#[derive(Debug, Clone, Copy, PartialEq, Eq, TokenParser)]
pub enum Token<'a> {
    LParen,
    RParen,
    Ident(&'a str),
    String(&'a str),
    Number(i32),
    Pipe,
}

use parser::*;
use token_combinator::*;

#[test]
fn tuple_test() {
    let tokens = &[
        Token::Ident("hoge"),
        Token::Number(10),
        Token::String("piyo"),
    ];
    let (tokens, (a, b, c)) = tuple((ident, number, string))(tokens).unwrap();
    assert!(tokens.is_empty());
    assert_eq!(a, "hoge");
    assert_eq!(b, 10);
    assert_eq!(c, "piyo");
}


#[test]
fn permutation_test() {
    let tokens = &[
        Token::Ident("hoge"),
        Token::Number(10),
        Token::String("piyo"),
    ];
    let (tokens, (a, b, c)) = permutation((number, string, ident))(tokens).unwrap();
    assert!(tokens.is_empty());
    assert_eq!(a, 10);
    assert_eq!(c, "hoge");
    assert_eq!(b, "piyo");
}

#[test]
fn alt_test() {
    let tokens = &[Token::Ident("ident")];
    let (rest, ident_str) = alt((ident, string))(tokens).unwrap();
    assert!(rest.is_empty());
    assert_eq!(ident_str, "ident");

    assert_eq!(
        alt((tuple((string, string)), tuple((ident, string))))(&[
            Token::Ident("fn"),
            Token::Ident("add")
        ]),
        Err(TokenParseError {
            tokens_consumed: 1,
            // returns most token-consumed result
            errors: vec![TokenParseErrorKind::Expects {
                expects: "string",
                found: Token::Ident("add")
            }]
        })
    );

    assert_eq!(
        alt((tuple((ident, string)), tuple((string, string))))(&[
            Token::Ident("fn"),
            Token::Ident("add")
        ]),
        Err(TokenParseError {
            tokens_consumed: 1,
            // returns most token-consumed result
            errors: vec![TokenParseErrorKind::Expects {
                expects: "string",
                found: Token::Ident("add")
            }]
        })
    );
}

#[test]
fn opt_test() {
    let tokens = &[Token::Ident("ident")];
    let (rest, ident_str) = opt(ident)(tokens).unwrap();
    assert!(rest.is_empty());
    assert_eq!(ident_str, Some("ident"));

    let (rest, ident_str) = opt(string)(tokens).unwrap();
    assert!(rest.len() == 1);
    assert_eq!(ident_str, None);
}

#[test]
fn delimited_test() {
    let tokens = &[Token::LParen, Token::Ident("a"), Token::RParen];
    let (rest, ident_str) = delimited(l_paren, ident, r_paren)(tokens).unwrap();
    assert!(rest.is_empty());
    assert_eq!(ident_str, "a");
}

#[test]
fn preceded_test() {
    let tokens = &[Token::LParen, Token::Ident("a")];
    let (rest, ident_str) = preceded(l_paren, ident)(tokens).unwrap();
    assert!(rest.is_empty());
    assert_eq!(ident_str, "a");
}

#[test]
fn terminated_test() {
    let tokens = &[Token::Ident("a"), Token::RParen];
    let (rest, ident_str) = terminated(ident, r_paren)(tokens).unwrap();
    assert!(rest.is_empty());
    assert_eq!(ident_str, "a");
}

#[test]
fn many0_test() {
    let tokens = &[Token::RParen];
    let (tokens, idents) = many0(ident)(tokens).unwrap();
    assert_eq!(tokens, &[Token::RParen]);
    assert!(idents.is_empty());

    let tokens = &[
        Token::Ident("a"),
        Token::Ident("b"),
        Token::Ident("c"),
        Token::RParen,
    ];
    let (tokens, idents) = many0(ident)(tokens).unwrap();
    assert_eq!(tokens, &[Token::RParen]);
    assert_eq!(idents, &["a", "b", "c"]);
}

#[test]
fn many1_test() {
    let tokens = &[
        Token::Ident("a"),
        Token::Ident("b"),
        Token::Ident("c"),
        Token::RParen,
    ];
    let (tokens, idents) = many1(ident)(tokens).unwrap();
    assert_eq!(tokens, &[Token::RParen]);
    assert_eq!(idents, &["a", "b", "c"]);
}

#[test]
fn separated_list0_test() {
    let tokens = &[Token::LParen];
    let (tokens, vec) = separated_list0(pipe, ident)(tokens).unwrap();
    assert_eq!(tokens.len(), 1);
    assert!(vec.is_empty());

    let tokens = &[
        Token::Ident("a"),
        Token::Pipe,
        Token::Ident("b"),
        Token::Pipe,
        Token::Ident("c"),
    ];
    let (tokens, vec) = separated_list0(pipe, ident)(tokens).unwrap();
    assert!(tokens.is_empty());
    assert_eq!(vec, vec!["a", "b", "c"]);
}

#[test]
fn separated_list1_test() {
    let tokens = &[Token::LParen];
    assert_eq!(
        separated_list1(pipe, ident)(tokens),
        Err(TokenParseError {
            errors: vec![TokenParseErrorKind::Expects { expects: "ident", found: Token::LParen }],
            tokens_consumed: 0
        })
    );

    let tokens = &[
        Token::Ident("a"),
        Token::Pipe,
        Token::Ident("b"),
        Token::Pipe,
        Token::Ident("c"),
    ];
    let (tokens, vec) = separated_list1(pipe, ident)(tokens).unwrap();
    assert!(tokens.is_empty());
    assert_eq!(vec, vec!["a", "b", "c"]);
}

#[test]
fn map_test() {
    #[derive(Debug, PartialEq, Eq)]
    enum AST {
        Ident(String)
    }
    let tokens = &[Token::Ident("myon")];
    let (_, result) = map(ident, |ident| AST::Ident(ident.to_string()))(tokens).unwrap();
    assert_eq!(
        result,
        AST::Ident("myon".to_string())
    )
}
