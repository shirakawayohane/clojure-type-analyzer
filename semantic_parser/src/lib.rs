mod semantic_ast;

use std::{collections::HashMap, vec};

use anyhow::{anyhow, ensure, Context, Result};
use location::Located;
use parser::{
    ast::{
        parser::{float_literal, integer_literal, keyword, symbol, vector},
        Symbol,
    },
    AST,
};
use paste::paste;
use semantic_ast::*;
use token_combinator::{
    alt, many0, map, map_res, opt, success, tuple, TokenParseError, TokenParseErrorKind,
    TokenParseResult, TokenParser,
};

type ASTParseResult<'a, O> = TokenParseResult<'a, AST<'a>, Located<O>, Located<AST<'a>>>;

macro_rules! specific_symbol {
    ($name: tt, $sym_name: expr, $expect: expr) => {
        paste! {
            fn [<$name _symbol>]<'a>(
                forms: &'a [Located<AST<'a>>],
                require_context: &RequireDef,
            ) -> ASTParseResult<'a, &'a Symbol<'a>> {
                located(map_res(symbol, |result| match result {
                    Ok((rest, sym)) => {
                        if sym.name == $sym_name {
                            Ok((rest, sym))
                        } else {
                            Err(TokenParseError {
                                errors: vec![TokenParseErrorKind::Expects {
                                    expects: $expect,
                                    found: forms[0].value.clone(),
                                }],
                                tokens_consumed: 0,
                            })
                        }
                    }
                    err => err,
                }))(forms)
            }
        }
    };
}

specific_symbol!(if, "if", "if");
specific_symbol!(when, "when", "when");
specific_symbol!(defn, "defn", "defn or schema.core/defn");
specific_symbol!(let, "let", "let or schema.core/let");

fn located<'a, O>(
    mut parser: impl TokenParser<'a, AST<'a>, O, Located<AST<'a>>>,
) -> impl FnMut(&'a [Located<AST<'a>>]) -> TokenParseResult<'a, AST<'a>, Located<O>, Located<AST<'a>>>
{
    move |forms: &'a [Located<AST<'a>>]| {
        let from = forms[0].range;
        let (rest, output) = parser.parse(forms)?;
        let to = rest.get(0).unwrap_or(forms.last().unwrap()).range;
        Ok((
            rest,
            Located {
                range: (from.0, to.1),
                value: output,
            },
        ))
    }
}

fn parse_require(list: AST) -> Result<RequireDef> {
    let forms = list.list_or_none().context("This form is not list")?;
    ensure!(
        forms.len() >= 2,
        "require form must contain at least 2 forms"
    );
    let mut aliases: HashMap<String, String> = HashMap::new();
    let keyword = forms.get(0).unwrap().keyword_or_none().unwrap();
    if keyword.name != "require" {
        return Err(anyhow!("This is not require form"));
    } else {
        for form in &forms[1..] {
            if let AST::Vector(require_vec) = &form.value {
                if require_vec.len() == 3 {
                    let first = require_vec.get(0).unwrap();
                    let second = require_vec.get(1).unwrap();
                    let third = require_vec.get(2).unwrap();
                    if let AST::Keyword(as_key) = &second.value {
                        if as_key.name == "as" {
                            if let AST::Symbol(ns_sym) = &first.value {
                                if let AST::Symbol(alias_sym) = &third.value {
                                    aliases.insert(
                                        ns_sym.name.to_string(),
                                        alias_sym.name.to_string(),
                                    );
                                    continue;
                                }
                            }
                        }
                    } else {
                        return Err(anyhow!("Failed to read require form"));
                    }
                }
            } else {
                return Err(anyhow!("Failed to read require form"));
            }
        }
    }
    Ok(RequireDef { aliases })
}

fn parse_type<'a>(forms: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, Type> {
    located(alt((
        map(symbol, |sym| Type::Scalar(sym.name.to_string())),
        map_res(vector, |res| match res {
            Ok((rest, vec)) => {
                if vec.len() != 1 {
                    return Err(TokenParseError {
                        errors: vec![TokenParseErrorKind::Other(
                            "Array type elements must be only one",
                        )],
                        tokens_consumed: 0,
                    });
                }
                let (rest, inner_type) = parse_type(rest)?;
                Ok((rest, Type::Array(Box::new(inner_type.value))))
            }
            Err(err) => Err(err),
        }),
    )))(forms)
}

fn parse_annotation<'a>(forms: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, Type> {
    let (rest, keyword) = keyword(forms)?;
    if keyword.name != "-" {
        return Err(TokenParseError {
            errors: vec![TokenParseErrorKind::Expects {
                expects: "-",
                found: forms[0].value.clone(),
            }],
            tokens_consumed: 0,
        });
    }
    let (rest, ty) = parse_type(rest)?;

    Ok((rest, ty))
}

fn parse_binding<'a>(forms: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, Binding> {
    located(map(symbol, |sym| Binding::Simple(sym.name.to_string())))(forms)
}

pub fn parse_function_decl<'a>(
    list: &'a AST<'a>,
) -> Result<(&'a [Located<AST<'a>>], FunctionDecl)> {
    let forms = list
        .list_or_none()
        .context("function should be at least a list")?;
    ensure!(
        forms.len() >= 3,
        "defn form should contain at least 3 forms"
    );
    let (rest, _) = defn_symbol(forms)
        .map_err(|_| anyhow!("Function definition should starts with 'defn' like symbol "))?;

    let (rest, name_sym) =
        symbol(rest).map_err(|_| anyhow!("Function name must be defined by symbol"))?;
    let function_name = name_sym.name;
    let (rest, return_annotation) =
        located(opt(|forms| parse_annotation(forms)))(rest).map_err(|_| anyhow!("Unreachable"))?;
    let (rest, args_vec) = vector(rest).map_err(|_| anyhow!("Expect arguments vector here"))?;
    let (_, args) = located(many0(tuple((
        parse_binding,
        opt(|forms| parse_annotation(forms)),
    ))))(&args_vec)
    .map_err(|_| anyhow!("Failed to parse args vector"))?;

    let func_decl = FunctionDecl {
        name: function_name.to_string(),
        arguments: args.value,
        return_type: return_annotation.value,
    };
    Ok((rest, func_decl))
}

fn parse_expression<'a>(
    forms: &'a [Located<AST<'a>>],
    require_context: &'a RequireDef,
) -> ASTParseResult<'a, Expression<'a>> {
    located(alt((
        map(integer_literal, |v| Expression::IntLiteral(*v)),
        map(float_literal, |v| Expression::FloatLiteral(*v)),
        map(parser::ast::parser::set, |forms| {
            let (_, exprs) =
                many0(|forms| parse_expression(forms, require_context))(forms).unwrap();
            Expression::SetLiteral(exprs)
        }),
        map_res(parser::ast::parser::vector, |res| match res {
            Ok((rest, forms)) => {
                let (_, exprs) =
                    many0(|forms| parse_expression(forms, require_context))(forms).unwrap();
                Ok((rest, Expression::VectorLiteral(exprs)))
            }
            Err(err) => Err(err),
        }),
        map_res(parser::ast::parser::set, |res| match res {
            Ok((rest, forms)) => {
                let (_, exprs) =
                    many0(|forms| parse_expression(forms, require_context))(forms).unwrap();
                Ok((rest, Expression::SetLiteral(exprs)))
            }
            Err(err) => Err(err),
        }),
        map_res(parser::ast::parser::list, |res| match res {
            Ok((rest, forms)) => {
                if forms.len() == 0 {
                    return Ok((rest, Expression::Unknown));
                }
                if let Ok((rest, _)) = if_symbol(forms, require_context) {
                    return map(
                        tuple((
                            |forms| parse_expression(forms, require_context),
                            |forms| parse_expression(forms, require_context),
                            opt(|forms| parse_expression(forms, require_context)),
                        )),
                        |(cond, if_true, opt_if_false)| {
                            Expression::If(IfExpression {
                                cond: Box::new(cond),
                                when_true: Box::new(if_true),
                                when_false: opt_if_false.map(|x| Box::new(x)),
                            })
                        },
                    )(rest);
                }
                if let Ok((rest, _)) = when_symbol(forms, require_context) {
                    return map(
                        tuple((
                            |forms| parse_expression(forms, require_context),
                            |forms| parse_expression(forms, require_context),
                        )),
                        |(cond, if_true)| {
                            Expression::When(WhenExpression {
                                cond: Box::new(cond),
                                when_true: Box::new(if_true),
                            })
                        },
                    )(rest);
                }

                todo!()
            }
            Err(err) => Err(err),
        }),
        map(success, |_| Expression::Unknown),
    )))(forms)
    // Ok((&forms[1..], match forms[0].unwrap_token() {
    //     AST::IntegerLiteral(i) => Expression::IntLiteral(*i),
    //     AST::FloatLiteral(f) => Expression::FloatLiteral(*f),
    //     AST::StringLiteral(_) => todo!(),
    //     AST::RegexLiteral(_) => todo!(),
    //     AST::List(_) => todo!(),
    //     AST::Vector(_) => todo!(),
    //     AST::Set(set) => Expression::SetLiteral(set),
    //     AST::Map(kvs) => Expression::MapLiteral(kvs),
    //     AST::Symbol(sym) => Expression::SymbolRef(sym),
    //     AST::Keyword(key) => Expression::Keyword(key),
    //     AST::Quoted(_) => todo!(),
    //     AST::SyntaxQuoted(_) => todo!(),
    //     AST::Root(_) => unreachable!(),
    // }))
}
