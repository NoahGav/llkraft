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
    Terminal(String),
    Alias(String),
    Leaf(String),
}

#[derive(Debug, Clone)]
pub enum GrammarPath {
    Optional(Box<GrammarRule>),
    Sequence(LinkedList<GrammarRule>),
    Choice(Vec<GrammarRule>),
}

pub struct GrammarBuilder {
    entry_name: Option<String>,
    pub rules: HashMap<String, GrammarRule>,
}

impl GrammarBuilder {
    pub fn new() -> Self {
        Self {
            entry_name: None,
            rules: HashMap::new(),
        }
    }

    pub fn entry_rule(&mut self, alias: &str, rule: GrammarRule) {
        self.entry_name = Some(alias.into());
        self.rules.insert(alias.into(), rule);
    }

    pub fn add_rule(&mut self, alias: &str, rule: GrammarRule) {
        self.rules.insert(alias.into(), rule);
    }

    pub fn to_digraph(&self) -> DiGraph<GrammarNode, ()> {
        let mut graph = DiGraph::new();

        let entry_name = self.entry_name.clone().unwrap();
        let entry = self.rules.get(&entry_name).unwrap();
        let root = graph.add_node(GrammarNode::Root(entry_name));
        let mut cache = HashMap::new();

        traverse_rule(&mut graph, &mut cache, &self.rules, root, entry, None);

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
) {
    match rule {
        GrammarRule::Node(node) => {
            traverse_node(graph, cache, rules, parent, node.clone(), continuation)
        }
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
) {
    if let Some(node) = cache.get(&ByAddress(node.clone())) {
        graph.add_edge(parent, node.clone(), ());
        return;
    }

    match node.as_ref() {
        GrammarNode::Terminal(_) => {
            let node_idx = graph.add_node(node.as_ref().clone());
            graph.add_edge(parent, node_idx, ());
            cache.insert(ByAddress(node), node_idx);

            if let Some(continuation) = continuation {
                let sequence = GrammarRule::Path(GrammarPath::Sequence(continuation));
                traverse_rule(graph, cache, rules, node_idx, &sequence, None);
            }
        }
        GrammarNode::Alias(alias) => {
            let rule = rules.get(alias).unwrap();
            traverse_rule(graph, cache, rules, parent, rule, continuation);
        }
        GrammarNode::Leaf(_) => {
            let node_idx = graph.add_node(node.as_ref().clone());
            graph.add_edge(parent, node_idx, ());
            cache.insert(ByAddress(node), node_idx);
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
                )
            }

            traverse_rule(graph, cache, rules, parent, rule, continuation);
        }
        GrammarPath::Sequence(elements) => {
            if elements.len() > 0 {
                let rule = elements.front().unwrap();
                let mut new_continuation = elements.clone().split_off(1);

                if let Some(continuation) = continuation {
                    new_continuation.extend(continuation);
                }

                traverse_rule(graph, cache, rules, parent, rule, Some(new_continuation));
            }
        }
        GrammarPath::Choice(options) => {
            for option in options {
                traverse_rule(graph, cache, rules, parent, option, continuation.clone());
            }
        }
    }
}

// TODO: Should fix macros needing caller to import crates.
#[macro_export]
macro_rules! terminal {
    ($token:expr) => {
        GrammarRule::Node(Rc::new(GrammarNode::Terminal($token.into())))
    };
}

#[macro_export]
macro_rules! alias {
    ($alias:expr) => {
        GrammarRule::Node(Rc::new(GrammarNode::Alias($alias.into())))
    };
}

#[macro_export]
macro_rules! leaf {
    ($alias:expr) => {
        GrammarRule::Node(GrammarNode::Leaf($alias.into()))
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
        let mut list = LinkedList::new();

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
