#![feature(rustc_attrs)]
pub mod token;

pub use token::Token;

use location::{Located, Location, Span};
use nom::{
    branch::{alt, permutation},
    bytes::complete::tag,
    bytes::complete::{take, take_till, take_till1},
    character::complete::{
        char, digit0, digit1, hex_digit1, line_ending, multispace1, oct_digit1, one_of, space1,
    },
    combinator::{eof, map, map_res, not, opt, recognize},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated, tuple},
    IResult, Parser,
};

use nom_locate::position;

type TokenizeResult<'a> = IResult<Span<'a>, Located<Token<'a>>>;

fn comment(input: Span) -> IResult<Span, ()> {
    map(
        permutation((
            char(';'),
            take_till(|c: char| c == '\r' || c == '\n'),
            alt((line_ending, eof)),
        )),
        |(_, _, _)| (),
    )(input)
}

fn skip0(input: Span) -> IResult<Span, ()> {
    map(many0(alt((comment, map(multispace1, |_| ())))), |_| ())(input)
}

fn located<'a, O>(
    mut parser: impl Parser<Span<'a>, O, nom::error::Error<Span<'a>>>,
) -> impl FnMut(Span<'a>) -> IResult<Span, Located<O>> {
    move |input: Span<'a>| {
        let (s, from) = position(input)?;
        let (s, output) = parser.parse(s)?;
        let (s, to) = position(s)?;
        Ok((
            s,
            Located {
                range: (
                    Location {
                        col: from.get_column() as u32,
                        line: from.location_line() as u32,
                        offset: from.location_offset() as u32,
                    },
                    Location {
                        col: (to.get_column() + to.len()) as u32,
                        line: to.location_line() as u32,
                        offset: to.location_offset() as u32,
                    },
                ),
                value: output,
            },
        ))
    }
}

fn lparen(input: Span) -> TokenizeResult {
    located(map(char('('), |_| Token::LParen))(input)
}
fn rparen(input: Span) -> TokenizeResult {
    located(map(char(')'), |_| Token::RParen))(input)
}
fn lbracket(input: Span) -> TokenizeResult {
    located(map(char('['), |_| Token::LBracket))(input)
}
fn rbracket(input: Span) -> TokenizeResult {
    located(map(char(']'), |_| Token::RBracket))(input)
}
fn lbrace(input: Span) -> TokenizeResult {
    located(map(char('{'), |_| Token::LBrace))(input)
}
fn rbrace(input: Span) -> TokenizeResult {
    located(map(char('}'), |_| Token::RBrace))(input)
}

fn char_literal(input: Span) -> TokenizeResult {
    fn anychar(s: Span) -> IResult<Span, char> {
        map(take(1usize), |s: Span| s.chars().next().unwrap())(s)
    }
    located(map(preceded(char('\\'), anychar), |c| {
        Token::CharLiteral(c)
    }))(input)
}

fn string_literal(input: Span) -> TokenizeResult {
    located(map(
        delimited(char('"'), take_till(|c| c == '"'), char('"')),
        |span| Token::StringLiteral(span),
    ))(input)
}

fn integer(i: Span) -> TokenizeResult {
    fn decimal_integer(input: Span) -> TokenizeResult {
        located(map(
            map_res(
                recognize(tuple((one_of("0123456789"), digit0))),
                |n: Span| i64::from_str_radix(n.fragment(), 10),
            ),
            |i| Token::IntegerLiteral(i),
        ))(input)
    }

    fn hex_integer(input: Span) -> TokenizeResult {
        located(map(
            map_res(preceded(tag("0x"), hex_digit1), |n: Span| {
                i64::from_str_radix(n.fragment(), 16)
            }),
            |i| Token::IntegerLiteral(i),
        ))(input)
    }

    fn oct_integer(input: Span) -> TokenizeResult {
        located(map(
            map_res(preceded(tag("0x"), oct_digit1), |n: Span| {
                i64::from_str_radix(n.fragment(), 8)
            }),
            |i| Token::IntegerLiteral(i),
        ))(input)
    }

    fn bin_integer(input: Span) -> TokenizeResult {
        located(map(
            map_res(
                preceded(tag("0b"), recognize(many1(one_of("01")))),
                |n: Span| i64::from_str_radix(n.fragment(), 2),
            ),
            |i| Token::IntegerLiteral(i),
        ))(input)
    }
    alt((decimal_integer, hex_integer, bin_integer, oct_integer))(i)
}

fn float(input: Span) -> TokenizeResult {
    located(map(
        map_res(recognize(tuple((digit1, char('.'), digit0))), |n: Span| {
            n.fragment().parse::<f64>()
        }),
        |v| Token::FloatLiteral(v),
    ))(input)
}

fn hat(input: Span) -> TokenizeResult {
    located(map(char('^'), |_| Token::Hat))(input)
}

fn sharp_underscore(input: Span) -> TokenizeResult {
    located(map(tag("#_"), |_| Token::SharpUnderescore))(input)
}

fn sharp(input: Span) -> TokenizeResult {
    located(map(char('#'), |_| Token::Sharp))(input)
}

fn and(input: Span) -> TokenizeResult {
    located(map(terminated(char('&'), space1), |_| Token::And))(input)
}

fn quote(input: Span) -> TokenizeResult {
    located(map(char('\''), |_| Token::Quote))(input)
}

fn syntax_quote(input: Span) -> TokenizeResult {
    located(map(char('`'), |_| Token::SyntaxQuote))(input)
}

fn tilde_at(input: Span) -> TokenizeResult {
    located(map(tag("~@"), |_| Token::TildeAt))(input)
}

fn tilde(input: Span) -> TokenizeResult {
    located(map(char('~'), |_| Token::Tilde))(input)
}

fn name(input: Span) -> IResult<Span, Span> {
    recognize(preceded(
        not(digit1),
        take_till1(|x: char| !x.is_alphanumeric() && !"*+!-_?.<>%=$".contains(x)),
    ))(input)
}

fn keyword(input: Span) -> TokenizeResult {
    located(map(
        recognize(permutation((
            char(':'),
            opt(char(':')),
            opt(terminated(name, char('/'))),
            name,
        ))),
        |s| Token::Keyword(s),
    ))(input)
}

fn symbol(input: Span) -> TokenizeResult {
    located(map(
        recognize(permutation((opt(terminated(name, char('/'))), name))),
        |s| Token::Symbol(s),
    ))(input)
}

pub fn tokenize<'a>(input: Span<'a>) -> IResult<Span, Vec<Located<Token<'a>>>> {
    let mut tokens = Vec::new();
    let mut rest = input;
    (rest, _) = skip0(rest)?;
    while rest.len() > 0 {
        let token: Located<Token>;
        (rest, token) = alt((
            lparen,
            rparen,
            lbracket,
            rbracket,
            lbrace,
            rbrace,
            quote,
            syntax_quote,
            hat,
            sharp_underscore,
            sharp,
            tilde_at,
            tilde,
            and,
            symbol,
            keyword,
            char_literal,
            string_literal,
            integer,
            float,
        ))(rest)?;
        tokens.push(token);
        (rest, _) = skip0(rest)?;
    }

    Ok((rest, tokens))
}
