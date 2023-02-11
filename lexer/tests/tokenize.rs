use std::fs::read_to_string;

use lexer::tokenize;

#[test]
fn tokenize_succeeds_on_various_sources() {
    let source = read_to_string("../examples/handler.clj").unwrap();
    tokenize(source.as_str().into()).unwrap();
}
