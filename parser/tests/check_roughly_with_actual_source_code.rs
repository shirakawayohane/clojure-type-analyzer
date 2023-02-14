use std::fs::read_to_string;

use lexer::tokenize;
use location::Span;
use parser::parse_root;

#[test]
fn tokenize_succeeds_on_various_sources() {
    let source = read_to_string("../examples/handler.clj").unwrap();
    let (_, tokens) = tokenize(Span::from(source.as_str())).unwrap();
    let root = parse_root(&tokens).unwrap();
    dbg!(root);
}
