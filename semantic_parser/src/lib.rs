pub mod semantic_ast;
use std::vec;

use anyhow::Result;
use location::{Located, Span};
use parser::{
    ast::{
        parser::{float_literal, integer_literal, keyword, list, string_literal, symbol, vector},
        Keyword, Symbol,
    },
    AST,
};
use paste::paste;
use semantic_ast::*;
use token_combinator::{
    alt, context, many0_until_end, map, map_res, opt, permutation, success, tuple, TokenParseError,
    TokenParseErrorKind, TokenParseResult, TokenParser,
};

type ASTParseResult<'a, O> = TokenParseResult<'a, AST<'a>, Located<O>, Located<AST<'a>>>;
type NotLocatedASTParseResult<'a, O> = TokenParseResult<'a, AST<'a>, O, Located<AST<'a>>>;

macro_rules! specific_symbol {
    ($name: tt, $sym_name: expr, $expect: expr) => {
        paste! {
            fn [<$name _symbol>]<'a>(
                forms: &'a [Located<AST<'a>>]
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

specific_symbol!(ns, "ns", "ns");
specific_symbol!(if, "if", "if");
specific_symbol!(when, "when", "when");
specific_symbol!(defn, "defn", "defn or schema.core/defn");
specific_symbol!(defschema, "defschema", "schema.core/defschema");
specific_symbol!(defmethod, "defmethod", "defmethod or schema.core/defmethod");
specific_symbol!(def, "def", "def or schema.core/def");
specific_symbol!(let, "let", "let or schema.core/let");

macro_rules! specific_keyword {
    ($name: tt, $key_name: expr, $expect: expr) => {
        paste! {
            fn [<$name _keyword>]<'a>(
                forms: &'a [Located<AST<'a>>]
            ) -> ASTParseResult<'a, &'a Keyword<'a>> {
                located(map_res(keyword, |result| match result {
                    Ok((rest, keyword)) => {
                        if keyword.name == $key_name {
                            Ok((rest, keyword))
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

specific_keyword!(as, "as", ":as");
specific_keyword!(refer, "refer", ":refer");
specific_keyword!(all, "all", ":all");
specific_keyword!(import, "impot", ":import");

fn located<'a, O>(
    mut parser: impl TokenParser<'a, AST<'a>, O, Located<AST<'a>>>,
) -> impl FnMut(&'a [Located<AST<'a>>]) -> TokenParseResult<'a, AST<'a>, Located<O>, Located<AST<'a>>>
{
    move |forms: &'a [Located<AST<'a>>]| {
        if forms.is_empty() {
            return Err(TokenParseError {
                errors: vec![TokenParseErrorKind::NotEnoughToken],
                tokens_consumed: 0,
            });
        }
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

fn parse_require<'a>(forms_in_ns_list: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, RequireDef> {
    fn parse_alias<'a>(forms_in_vec: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, Alias> {
        located(map(
            tuple((symbol, as_keyword, symbol)),
            |(ns_sym, _, alias_sym)| Alias {
                ns: ns_sym.name.to_string(),
                alias: alias_sym.name.to_string(),
            },
        ))(forms_in_vec)
    }
    fn parse_namespace_only<'a>(
        forms_in_vec: &'a [Located<AST<'a>>],
    ) -> ASTParseResult<'a, NamespaceOnly> {
        if forms_in_vec.len() != 0 {
            return Err(TokenParseError {
                errors: vec![TokenParseErrorKind::Other("Invalid require vector".to_owned())],
                tokens_consumed: 0,
            });
        }
        located(map(symbol, |sym| NamespaceOnly {
            ns: sym.name.to_string(),
        }))(forms_in_vec)
    }
    fn parse_refer_all<'a>(forms_in_vec: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, ReferAll> {
        located(map(
            tuple((symbol, refer_keyword, all_keyword)),
            |(ns_sym, _, _)| ReferAll {
                ns: ns_sym.name.to_string(),
            },
        ))(forms_in_vec)
    }
    fn parse_refers<'a>(forms_in_vec: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, Refers> {
        located(map(
            tuple((
                symbol,
                refer_keyword,
                map_res(vector, |res| match res {
                    Ok((_, forms_in_refer_vector)) => map(many0_until_end(symbol), |sym| {
                        sym.into_iter()
                            .map(|x| x.name.to_string())
                            .collect::<Vec<_>>()
                    })(forms_in_refer_vector),
                    Err(err) => Err(err),
                }),
            )),
            |(ns_sym, _, refered_symbols)| Refers {
                ns: ns_sym.name.to_string(),
                refers: refered_symbols,
            },
        ))(forms_in_vec)
    }

    let (rest, forms_in_require_list) = list(forms_in_ns_list)?;

    let (_, require_def) = context(
        "require",
        located(map(
            permutation((
                many0_until_end(parse_alias),
                many0_until_end(parse_namespace_only),
                many0_until_end(parse_refer_all),
                many0_until_end(parse_refers),
            )),
            |(aliases, namespace_onlys, refer_alls, refers)| RequireDef {
                aliases: aliases,
                namespace_onlys,
                refers,
                refer_alls,
            },
        )),
    )(&forms_in_require_list)?;

    Ok((rest, require_def))
}

fn parse_import<'a>(forms_in_ns_list: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, ImportDef> {
    let (rest, list_forms) = list(forms_in_ns_list)?;
    let (_, import_def) = context(
        "import",
        located(map(
            tuple((import_keyword, many0_until_end(success))),
            |_| ImportDef {},
        )),
    )(list_forms)?;

    Ok((rest, import_def))
}

fn parse_ns_def<'a>(top_forms: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, NamespaceDef> {
    let (rest, forms_in_list) = list(top_forms)?;
    let (_, ns_def) = context(
        "ns definition",
        located(map(
            tuple((
                ns_symbol,
                symbol,
                permutation((opt(parse_require), opt(parse_import))),
            )),
            |(_, ns_sym, (require, import))| NamespaceDef {
                namespace: ns_sym.name.to_string(),
                require,
                import,
            },
        )),
    )(forms_in_list)?;

    Ok((rest, ns_def))
}

fn parse_map_key<'a>(forms_in_map: &'a [Located<AST<'a>>]) -> NotLocatedASTParseResult<'a, MapKey> {
    map(parse_expression, |expr| match expr.value {
        Expression::IntLiteral(i) => MapKey::Integer(i),
        Expression::StringLiteral(s) => MapKey::String(s),
        Expression::Keyword(k) => MapKey::Keyword(k.fullname()),
        Expression::SymbolRef(s) => MapKey::Type(Type::Scalar(s.fullname())),
        _ => return MapKey::Unknown,
    })(forms_in_map)
}

fn parse_type<'a>(forms: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, Type> {
    fn parse_map_type<'a>(forms: &'a [Located<AST<'a>>]) -> NotLocatedASTParseResult<'a, Type> {
        map_res(parser::ast::parser::map, |res| match res {
            Ok((_, forms_in_map)) => {
                map(many0_until_end(tuple((parse_map_key, parse_type))), |kvs| {
                    Type::Map(
                        kvs.into_iter()
                            .map(|(k, v)| (k, v.value))
                            .collect::<Vec<_>>(),
                    )
                })(forms_in_map)
            }
            Err(err) => Err(err),
        })(forms)
    }
    let (_1, ty) = context(
        "type",
        located(alt((
            map(symbol, |sym| Type::Scalar(sym.name.to_string())),
            parse_map_type,
        ))),
    )(forms)?;

    Ok((&forms[1..], ty))
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
    context(
        "binding",
        located(map(symbol, |sym| Binding::Simple(sym.name.to_string()))),
    )(forms)
}

fn parse_expression<'a>(forms: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, Expression<'a>> {
    if forms.is_empty() {
        return Err(TokenParseError {
            errors: vec![TokenParseErrorKind::NotEnoughToken],
            tokens_consumed: 0,
        });
    }
    let (_, expr) = context(
        "expression",
        located(alt((
            map_res(parser::ast::parser::list, |res| match res {
                Ok((rest, forms)) => {
                    if forms.len() == 0 {
                        return Ok((rest, Expression::Unknown));
                    }
                    if let Ok((_, _)) = if_symbol(forms) {
                        return map(
                            tuple((parse_expression, parse_expression, opt(parse_expression))),
                            |(cond, if_true, opt_if_false)| {
                                Expression::If(IfExpression {
                                    cond: Box::new(cond),
                                    when_true: Box::new(if_true),
                                    when_false: opt_if_false.map(|x| Box::new(x)),
                                })
                            },
                        )(forms);
                    }
                    if let Ok((_, _)) = when_symbol(forms) {
                        return map(
                            tuple((parse_expression, parse_expression)),
                            |(cond, if_true)| {
                                Expression::When(WhenExpression {
                                    cond: Box::new(cond),
                                    when_true: Box::new(if_true),
                                })
                            },
                        )(forms);
                    }
                    if let Ok((rest, _)) = let_symbol(forms) {
                        let (rest, forms_in_vec) = vector(rest)?;
                        let (_, bindings) = many0_until_end(tuple((
                            parse_binding,
                            opt(parse_annotation),
                            parse_expression_and_box,
                        )))(&forms_in_vec)?;
                        let (_, body) = many0_until_end(parse_expression)(rest)?;
                        return Ok((
                            rest,
                            Expression::Let(LetExpression {
                                bindings: bindings,
                                body,
                            }),
                        ));
                    }
                    map(
                        tuple((parse_expression, many0_until_end(parse_expression))),
                        |(fn_exp, args)| {
                            Expression::Call(CallExpression {
                                func_expr: Box::new(fn_exp),
                                args,
                            })
                        },
                    )(forms)
                }
                Err(err) => Err(err),
            }),
            map(integer_literal, |v| Expression::IntLiteral(*v)),
            map(float_literal, |v| Expression::FloatLiteral(*v)),
            map(symbol, |sym| Expression::SymbolRef(sym)),
            map(keyword, |key| Expression::Keyword(key)),
            map_res(parser::ast::parser::vector, |res| match res {
                Ok((rest, forms)) => {
                    let (_, exprs) = many0_until_end(parse_expression)(forms)?;
                    Ok((rest, Expression::VectorLiteral(exprs)))
                }
                Err(err) => Err(err),
            }),
            map_res(parser::ast::parser::set, |res| match res {
                Ok((rest, forms)) => {
                    let (_, exprs) = many0_until_end(parse_expression)(forms)?;
                    Ok((rest, Expression::SetLiteral(exprs)))
                }
                Err(err) => Err(err),
            }),
            map_res(parser::ast::parser::map, |res| match res {
                Ok((rest, kvs)) => {
                    let (_, map_expr) = map(
                        many0_until_end(tuple((parse_expression, parse_expression))),
                        |kvs_expr| Expression::MapLiteral(kvs_expr),
                    )(kvs)?;
                    Ok((rest, map_expr))
                }
                Err(err) => Err(err),
            }),
            map(parser::ast::parser::list, |_| Expression::Unknown),
        ))),
    )(forms)?;

    Ok((&forms[1..], expr))
}

fn parse_expression_and_box<'a>(
    forms: &'a [Located<AST<'a>>],
) -> ASTParseResult<'a, Box<Expression<'a>>> {
    located(map(parse_expression, |expr| Box::new(expr.value)))(forms)
}

pub fn parse_function_decl<'a>(forms: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, FunctionDecl> {
    context(
        "function definition",
        located(map(
            tuple((
                defn_symbol,
                symbol,
                opt(string_literal), // doc string
                opt(parse_annotation),
                map_res(vector, |res| match res {
                    Ok((rest, args_vec)) => {
                        let (_, args) = many0_until_end(tuple((
                            parse_binding,
                            opt(parse_annotation),
                        )))(&args_vec)?;
                        Ok((rest, args))
                    }
                    Err(err) => Err(err),
                }),
            )),
            |(_, name_sym, _, opt_return_type, args)| FunctionDecl {
                name: name_sym.name.to_string(),
                return_type: opt_return_type,
                arguments: args,
            },
        )),
    )(forms)
}

fn parse_function<'a>(toplevel_forms: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, Function> {
    let (rest, forms_in_list) = list(toplevel_forms)?;
    let (_, func) = context(
        "function",
        located(map(
            tuple((parse_function_decl, many0_until_end(parse_expression))),
            |(decl, exprs)| Function {
                decl: decl.value,
                exprs,
            },
        )),
    )(forms_in_list)?;

    Ok((rest, func))
}

#[test]
fn parse_function_test() {
    let source = "
    (defn add :- s/Int
        [a :- s/Int
         b :- s/Int]
         (+ a b))
";

    let (_, tokens) = lexer::tokenize(Span::from(source)).unwrap();
    let (_, ast) = parser::parse_form(&tokens).unwrap();
    let forms_in_list = ast.list_or_none().unwrap();
    let source = parse_function(&forms_in_list);
    dbg!(&source);
}

fn parse_def<'a>(toplevel_forms: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, Define> {
    let (rest, forms_in_list) = list(toplevel_forms)?;
    let (_, def) = context(
        "def",
        located(map(
            tuple((
                def_symbol,
                symbol,
                opt(parse_annotation),
                opt(string_literal), // doc string
                parse_expression,
            )),
            |(_, name_sym, ty, _, value)| Define {
                name: name_sym.name,
                ty,
                value,
            },
        )),
    )(&forms_in_list)?;

    Ok((rest, def))
}

fn parse_defschema<'a>(toplevel_forms: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, DefSchema> {
    let (rest, forms_in_list) = list(toplevel_forms)?;
    let (_, def) = context(
        "defschema",
        located(map(
            tuple((defschema_symbol, symbol, opt(string_literal), parse_type)),
            |(_, name_sym, _, ty)| DefSchema {
                name: name_sym.name.to_string(),
                ty: ty.value,
            },
        )),
    )(&forms_in_list)?;

    Ok((rest, def))
}

fn parse_method<'a>(toplevel_forms: &'a [Located<AST<'a>>]) -> ASTParseResult<'a, Method> {
    // TODO: impl later
    let (rest, list_forms) = list(toplevel_forms)?;
    let (_, import_def) = context(
        "method",
        located(map(
            tuple((defmethod_symbol, many0_until_end(success))),
            |_| Method {},
        )),
    )(list_forms)?;

    Ok((rest, import_def))
}

fn parse_toplevel<'a>(toplevel_forms: &'a [Located<AST<'a>>]) -> ASTParseResult<TopLevel> {
    let (_, toplevel) = context(
        "toplevel",
        located(alt((
            map(parse_function, |func| TopLevel::Function(func.value)),
            map(parse_def, |def| TopLevel::Def(def.value)),
            map(parse_method, |method| TopLevel::Method(method.value)),
            map(parse_defschema, |schema| TopLevel::DefSchema(schema.value)),
            map(success, |_| TopLevel::Unknown),
        ))),
    )(toplevel_forms)?;

    Ok((&toplevel_forms[1..], toplevel))
}

fn parse_source_impl<'a>(toplevel_forms: &'a [Located<AST<'a>>]) -> ASTParseResult<Source<'a>> {
    located(map(
        tuple((parse_ns_def, many0_until_end(parse_toplevel))),
        |(ns_def, toplevels)| Source { ns_def, toplevels },
    ))(toplevel_forms)
}

pub fn parse_source<'a>(
    root_ast: &'a Located<AST<'a>>,
) -> Result<Source, TokenParseError<AST<'a>>> {
    if let AST::Root(toplevel_forms) = &root_ast.value {
        match parse_source_impl(&toplevel_forms) {
            Ok((_, source)) => {
                return Ok(source.value);
            }
            Err(err) => Err(err),
        }
    } else {
        panic!("Provided ast is not Root");
    }
}
