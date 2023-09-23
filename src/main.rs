use llkraft::*;
use petgraph::dot::{Config, Dot};
use std::rc::Rc;
use std::{collections::LinkedList, process::Command};

fn main() {
    let mut grammar = GrammarBuilder::new();

    grammar.entry_rule("entry", sequence!(alias!("decl"), terminal!("EOF")));

    grammar.add_rule(
        "decl",
        sequence!(
            optional!(terminal!("EXPORT")),
            choice!(alias!("fn"), alias!("struct"))
        ),
    );

    grammar.add_rule("fn", terminal!("FN"));
    grammar.add_rule("struct", terminal!("STRUCT"));

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
