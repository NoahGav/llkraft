use llkraft::*;
use petgraph::dot::{Config, Dot};
use petgraph::prelude::NodeIndex;
use std::process::Command;

fn generate(tree: &GrammarTree, root: NodeIndex, code: &mut String, depth: usize) {
    // TODO: Currently the generated code does not emit syntax nodes. The reason
    // TODO: is because I haven't added node_name or whatever to grammar nodes.

    let children: Vec<NodeIndex> = tree.graph.neighbors(root).collect();

    if children.len() > 1 {
        *code += &format!("let token_{} = eat();", depth);
        *code += "match token {";

        for child in children {
            let weight = tree.graph.node_weight(child).unwrap();

            match weight {
                GrammarNode::Token(token) => {
                    *code += &format!("TokenKind::{} => {{", token);
                    generate(tree, child, code, depth + 1);
                    *code += "}";
                }
                GrammarNode::Recursion(_) => todo!(),
                _ => unreachable!(),
            }
        }

        *code += "_ => todo!()";
        *code += "};";
    } else if children.len() > 0 {
        let child = children.first().unwrap().clone();
        let weight = tree.graph.node_weight(child).unwrap();

        match weight {
            GrammarNode::Token(token) => {
                *code += &format!("let token_{} = expect(TokenKind::{});", depth, token);
                generate(tree, child, code, depth + 1);
            }
            GrammarNode::Recursion(alias) => {
                *code += &format!("let {}_{} = parse_{}();", alias, depth, alias);
                generate(tree, child, code, depth + 1);
            }
            _ => unreachable!(),
        }
    }
}

fn main() {
    let mut grammar = GrammarBuilder::new();

    grammar.add_rule("expr_stmt", sequence!(alias!("expr"), token!("SEMICOLON")));

    grammar.add_rule(
        "expr",
        choice!(
            alias!("term"),
            sequence!(
                alias!("term"),
                choice!(token!("PLUS"), token!("MINUS")),
                recursion!("expr")
            )
        ),
    );

    grammar.add_rule(
        "term",
        sequence!(optional!(token!("MINUS")), token!("IDENT")),
    );

    let tree = grammar.to_digraph();
    let root = tree.roots.get("expr_stmt").unwrap().clone();

    let mut code = String::new();
    generate(&tree, root, &mut code, 0);
    println!("{}", code);

    std::fs::write(
        "output.dot",
        format!(
            "{:?}",
            Dot::with_config(&tree.graph, &[Config::EdgeNoLabel])
        ),
    )
    .unwrap();

    Command::new("dot")
        .arg("-Tsvg")
        .arg("-O")
        .arg("./output.dot")
        .spawn()
        .unwrap();
}
