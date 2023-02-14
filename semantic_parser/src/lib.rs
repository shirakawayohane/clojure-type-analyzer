mod semantic_ast;

use std::collections::HashMap;

use anyhow::{anyhow, ensure, Context, Result};
use nom::sequence::preceded;
use parser::AST;
use semantic_ast::*;
use token_combinator::{opt, TokenParseResult};

use crate::semantic_ast::Symbol;

type ASTParseResult<'a, O> = TokenParseResult<'a, AST<'a>, O>;

fn parse_require(list: AST) -> Result<RequireDef> {
    let forms = list.list_or_none().context("This form is not list")?;
    ensure!(
        forms.len() >= 2,
        "require form must contain at least 2 forms"
    );
    let mut aliases: HashMap<Symbol, Symbol> = HashMap::new();
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
                                    aliases.insert(ns_sym.into(), alias_sym.into());
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
    Ok(RequireDef {
        aliases
    })
}

const DEFN_SYMBOLS: [&str; 2] = ["defn", "schema.core/defn"];

fn parse_type<'a>(forms: &'a[AST<'a>]) -> ASTParseResult<'a, Type> {
    todo!()
}

fn parse_annotation<'a>(forms: &'a[AST<'a>]) -> ASTParseResult<'a, Type> {
    // preceded(parser::ast::parser::keyword, opt(parse_type))(forms)
    todo!()
}

fn parse_function_decl(list: AST, ns_context: HashMap<String, String>) -> Result<FunctionDecl> {
    let forms = list.list_or_none().context("function should be at least a list")?;
    ensure!(forms.len() >= 3, "defn form should contain at least 3 forms");
    let defn_key = forms.get(0).unwrap().symbol_or_none().expect("Function form must start with symbol 'defn'");
    let fullname = format!("{}/{}", ns_context.get(defn_key.name).map(|x| x.as_str()).unwrap_or(""), defn_key.name);
    ensure!(DEFN_SYMBOLS.contains(&fullname.as_str()), "Function form must start with symbol 'defn'");
    let name_sym = forms[1].symbol_or_none().context("Function name must be defined by symbol")?;
    let fucntion_name = name_sym.name;

    todo!()
}
