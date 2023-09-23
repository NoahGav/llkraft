use llkraft::{alias, choice, leaf, sequence, terminal};

use std::{
    collections::{HashMap, LinkedList},
    process::Command,
};

use petgraph::{
    dot::{Config, Dot},
    prelude::{DiGraph, NodeIndex},
};

#[derive(Debug, Clone)]
enum GrammarRule {
    Node(GrammarNode),
    Path(GrammarPath),
}

#[derive(Debug, Clone)]
enum GrammarNode {
    Root(String),
    Terminal(String),
    Alias(String),
    Leaf(String),
}

#[derive(Debug, Clone)]
enum GrammarPath {
    Sequence(LinkedList<GrammarRule>),
    Choice(Vec<GrammarRule>),
}

fn to_digraph(
    rules: &HashMap<String, GrammarRule>,
    entry_name: String,
) -> DiGraph<GrammarNode, ()> {
    let mut graph = DiGraph::new();

    let entry = rules.get(&entry_name).unwrap();
    let root = graph.add_node(GrammarNode::Root(entry_name));

    traverse_rule(&mut graph, rules, root, entry, None);

    graph
}

fn traverse_rule(
    graph: &mut DiGraph<GrammarNode, ()>,
    rules: &HashMap<String, GrammarRule>,
    parent: NodeIndex,
    rule: &GrammarRule,
    continuation: Option<LinkedList<GrammarRule>>,
) {
    match rule {
        GrammarRule::Node(node) => traverse_node(graph, rules, parent, node, continuation),
        GrammarRule::Path(path) => traverse_path(graph, rules, parent, path, continuation),
    }
}

fn traverse_node(
    graph: &mut DiGraph<GrammarNode, ()>,
    rules: &HashMap<String, GrammarRule>,
    parent: NodeIndex,
    node: &GrammarNode,
    continuation: Option<LinkedList<GrammarRule>>,
) {
    match node {
        GrammarNode::Terminal(_) => {
            let node = graph.add_node(node.clone());
            graph.add_edge(parent, node, ());

            if let Some(continuation) = continuation {
                let sequence = GrammarRule::Path(GrammarPath::Sequence(continuation));
                traverse_rule(graph, rules, node, &sequence, None);
            }
        }
        GrammarNode::Alias(alias) => {
            let rule = rules.get(alias).unwrap();
            traverse_rule(graph, rules, parent, rule, continuation);
        }
        GrammarNode::Leaf(_) => {
            let node = graph.add_node(node.clone());
            graph.add_edge(parent, node, ());
        }
        _ => unreachable!(),
    };
}

fn traverse_path(
    graph: &mut DiGraph<GrammarNode, ()>,
    rules: &HashMap<String, GrammarRule>,
    parent: NodeIndex,
    path: &GrammarPath,
    continuation: Option<LinkedList<GrammarRule>>,
) {
    match path {
        GrammarPath::Sequence(elements) => {
            if elements.len() > 0 {
                let rule = elements.front().unwrap();
                let mut new_continuation = elements.clone().split_off(1);

                if let Some(continuation) = continuation {
                    new_continuation.extend(continuation);
                }

                traverse_rule(graph, rules, parent, rule, Some(new_continuation));
            }
        }
        GrammarPath::Choice(options) => {
            for option in options {
                traverse_rule(graph, rules, parent, option, continuation.clone());
            }
        }
    }
}

fn main() {
    let mut rules = HashMap::new();

    rules.insert(
        "expr".into(),
        sequence!(alias!("term"), terminal!("PLUS"), leaf!("expr")),
    );

    rules.insert(
        "term".into(),
        choice!(
            terminal!("IDENT"),
            sequence!(terminal!("MINUS"), terminal!("IDENT"))
        ),
    );

    let graph = to_digraph(&rules, "expr".into());

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
