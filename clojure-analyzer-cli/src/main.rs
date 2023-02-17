use std::{env, fs};

use location::Span;

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();
    let file_path = args.get(1).unwrap();
    let source = fs::read_to_string(file_path)?;
    let (_, tokens) = lexer::tokenize(Span::new(&source)).unwrap();
    let (_, ast) = parser::parse_root(&tokens).unwrap();
    let semantic_root = semantic_parser::parse_source(&ast).unwrap();
    let errors = analyzer::analyze_source(semantic_root);
    for error in errors {
        println!(
            "{}:{}:{}: {}",
            file_path,
            error.loc.0.line,
            error.loc.0.col - 1,
            error.message
        );
    }
    Ok(())
}
