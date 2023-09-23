use llkraft::*;
use petgraph::dot::{Config, Dot};
use std::process::Command;

fn main() {
    let mut grammar = GrammarBuilder::new();

    grammar.add_rule(
        "expr",
        sequence!(alias!("term"), alias!("op"), recursion!("expr")),
    );

    grammar.add_rule("term", sequence!(optional!(token!("-")), token!("IDENT")));

    // grammar.add_rule(
    //     "term",
    //     choice!(token!("IDENT"), sequence!(token!("MINUS"), token!("IDENT"))),
    // );

    grammar.add_rule(
        "op",
        choice!(token!("+"), token!("-"), token!("*"), token!("/")),
    );

    let graph = grammar.to_digraph();

    // TODO: It is now finished. The graph produced represents the grammar
    // TODO: correctly. Now all we have to do is create a generator that
    // TODO: traverse this graph and generates the actual parser (also we
    // TODO: have to add rule names to nodes because that's important
    // TODO: to know what syntax node was finished being parsed).

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
