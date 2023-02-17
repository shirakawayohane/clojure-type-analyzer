use std::{error, fs::read_to_string};

use analyzer::analyze_source;
use lexer::tokenize;
use location::Span;
use parser::parse_root;
use semantic_parser::parse_source;

#[test]
fn analyze_error() {
    let source = read_to_string("../examples/simple-source-for-analyzer.clj").unwrap();
    let (_, tokens) = tokenize(Span::from(source.as_str())).unwrap();
    let (_, root_ast) = parse_root(&tokens).unwrap();
    let source = parse_source(&root_ast).unwrap();
    dbg!(&source);
    let errors = analyze_source(source);
    dbg!(&errors);
}
