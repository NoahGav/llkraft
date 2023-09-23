use llkraft::*;
use petgraph::dot::{Config, Dot};
use std::process::Command;

fn main() {
    let mut grammar = GrammarBuilder::new();

    grammar.add_rule(
        "params",
        sequence!(token!("L_PAREN"), alias!("params'"), token!("R_PAREN")),
    );

    grammar.add_rule(
        "params'",
        optional!(choice!(
            alias!("param"),
            sequence!(alias!("param"), token!("COMMA"), recursion!("params'"))
        )),
    );

    grammar.add_rule("param", token!("IDENT"));

    let graph = grammar.to_digraph();

    std::fs::write(
        "output.dot",
        format!("{:?}", Dot::with_config(&graph, &[Config::EdgeNoLabel])),
    )
    .unwrap();

    Command::new("dot")
        .arg("-Tsvg")
        .arg("-O")
        .arg("./output.dot")
        .spawn()
        .unwrap();
}
