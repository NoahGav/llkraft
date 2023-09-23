use llkraft::*;
use petgraph::dot::{Config, Dot};
use std::{collections::LinkedList, process::Command};

fn main() {
    let mut grammar = GrammarBuilder::new();

    grammar.entry_rule(
        "expr".into(),
        sequence!(alias!("term"), terminal!("PLUS"), leaf!("expr")),
    );

    grammar.add_rule(
        "term".into(),
        choice!(
            terminal!("IDENT"),
            sequence!(terminal!("MINUS"), terminal!("IDENT"))
        ),
    );

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
