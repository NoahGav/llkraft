use by_address::ByAddress;
use petgraph::prelude::{DiGraph, NodeIndex};

use std::{
    collections::{HashMap, LinkedList},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub enum GrammarRule {
    Node(Rc<GrammarNode>),
    Path(GrammarPath),
}

#[derive(Debug, Clone)]
pub enum GrammarNode {
    Root(String),
    Token(String),
    Alias(String),
    Recursion(String),
}

#[derive(Debug, Clone)]
pub enum GrammarPath {
    Optional(Box<GrammarRule>),
    Sequence(LinkedList<GrammarRule>),
    Choice(Vec<GrammarRule>),
}

pub struct GrammarBuilder {
    pub rules: HashMap<String, GrammarRule>,
}

impl GrammarBuilder {
    pub fn new() -> Self {
        Self {
            rules: HashMap::new(),
        }
    }

    pub fn add_rule(&mut self, alias: &str, rule: GrammarRule) {
        self.rules.insert(alias.into(), rule);
    }

    pub fn to_digraph(&self) -> DiGraph<GrammarNode, ()> {
        let mut graph = DiGraph::new();

        for (alias, rule) in &self.rules {
            let root = graph.add_node(GrammarNode::Root(alias.clone()));
            let mut cache = HashMap::new();

            traverse_rule(&mut graph, &mut cache, &self.rules, root, rule, None, false);
        }

        graph
    }
}

fn traverse_rule(
    graph: &mut DiGraph<GrammarNode, ()>,
    cache: &mut HashMap<ByAddress<Rc<GrammarNode>>, NodeIndex>,
    rules: &HashMap<String, GrammarRule>,
    parent: NodeIndex,
    rule: &GrammarRule,
    continuation: Option<LinkedList<GrammarRule>>,
    start_choice: bool,
) {
    match rule {
        GrammarRule::Node(node) => traverse_node(
            graph,
            cache,
            rules,
            parent,
            node.clone(),
            continuation,
            start_choice,
        ),
        GrammarRule::Path(path) => traverse_path(graph, cache, rules, parent, path, continuation),
    }
}

fn traverse_node(
    graph: &mut DiGraph<GrammarNode, ()>,
    cache: &mut HashMap<ByAddress<Rc<GrammarNode>>, NodeIndex>,
    rules: &HashMap<String, GrammarRule>,
    parent: NodeIndex,
    node: Rc<GrammarNode>,
    continuation: Option<LinkedList<GrammarRule>>,
    start_choice: bool,
) {
    if let Some(node) = cache.get(&ByAddress(node.clone())) {
        graph.add_edge(parent, node.clone(), ());

        // TODO: Make sure this isn't a problem (removing it).
        // if let Some(continuation) = continuation {
        //     let sequence = GrammarRule::Path(GrammarPath::Sequence(continuation));
        //     traverse_rule(graph, cache, rules, node.clone(), &sequence, None, false);
        // }

        return;
    }

    match node.as_ref() {
        GrammarNode::Token(_) => {
            let node_idx = graph.add_node(node.as_ref().clone());
            graph.add_edge(parent, node_idx, ());

            if !start_choice {
                cache.insert(ByAddress(node), node_idx);
            }

            if let Some(continuation) = continuation {
                let sequence = GrammarRule::Path(GrammarPath::Sequence(continuation));
                traverse_rule(graph, cache, rules, node_idx, &sequence, None, false);
            }
        }
        GrammarNode::Alias(alias) => {
            let rule = rules.get(alias).unwrap();
            traverse_rule(
                graph,
                cache,
                rules,
                parent,
                rule,
                continuation,
                start_choice,
            );
        }
        GrammarNode::Recursion(_) => {
            let node_idx = graph.add_node(node.as_ref().clone());
            graph.add_edge(parent, node_idx, ());

            if !start_choice {
                cache.insert(ByAddress(node), node_idx);
            }

            if let Some(continuation) = continuation {
                let sequence = GrammarRule::Path(GrammarPath::Sequence(continuation));
                traverse_rule(graph, cache, rules, node_idx, &sequence, None, false);
            }
        }
        _ => unreachable!(),
    };
}

fn traverse_path(
    graph: &mut DiGraph<GrammarNode, ()>,
    cache: &mut HashMap<ByAddress<Rc<GrammarNode>>, NodeIndex>,
    rules: &HashMap<String, GrammarRule>,
    parent: NodeIndex,
    path: &GrammarPath,
    continuation: Option<LinkedList<GrammarRule>>,
) {
    match path {
        GrammarPath::Optional(rule) => {
            if let Some(continuation) = continuation.clone() {
                traverse_rule(
                    graph,
                    cache,
                    rules,
                    parent,
                    continuation.front().unwrap(),
                    Some(continuation.clone().split_off(1)),
                    false,
                )
            }

            traverse_rule(graph, cache, rules, parent, rule, continuation, false);
        }
        GrammarPath::Sequence(elements) => {
            if elements.len() > 0 {
                let rule = elements.front().unwrap();
                let mut new_continuation = elements.clone().split_off(1);

                if let Some(continuation) = continuation {
                    new_continuation.extend(continuation);
                }

                traverse_rule(
                    graph,
                    cache,
                    rules,
                    parent,
                    rule,
                    Some(new_continuation),
                    false,
                );
            }
        }
        GrammarPath::Choice(options) => {
            for option in options {
                traverse_rule(
                    graph,
                    cache,
                    rules,
                    parent,
                    option,
                    continuation.clone(),
                    true,
                );
            }
        }
    }
}

#[macro_export]
macro_rules! token {
    ($token:expr) => {
        GrammarRule::Node(std::rc::Rc::new(GrammarNode::Token($token.into())))
    };
}

#[macro_export]
macro_rules! alias {
    ($alias:expr) => {
        GrammarRule::Node(std::rc::Rc::new(GrammarNode::Alias($alias.into())))
    };
}

#[macro_export]
macro_rules! recursion {
    ($alias:expr) => {
        GrammarRule::Node(std::rc::Rc::new(GrammarNode::Recursion($alias.into())))
    };
}

#[macro_export]
macro_rules! optional {
    ($rule:expr) => {
        GrammarRule::Path(GrammarPath::Optional(Box::new($rule)))
    };
}

#[macro_export]
macro_rules! sequence {
    ($($x:expr),*) => {{
        let mut list = std::collections::LinkedList::new();

        $(
            list.push_back($x);
        )+

        GrammarRule::Path(GrammarPath::Sequence(list))
    }};
}

#[macro_export]
macro_rules! choice {
    ($($x:expr),*) => {
        GrammarRule::Path(GrammarPath::Choice(vec![$($x),*]))
    };
}
