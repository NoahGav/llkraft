use by_address::ByAddress;
use petgraph::prelude::{DiGraph, NodeIndex};

use std::{
    collections::{HashMap, HashSet, LinkedList},
    fmt::Debug,
    rc::Rc,
};

#[derive(Debug, Clone)]
pub enum GrammarRule {
    Node(Rc<GrammarNode>),
    Path(GrammarPath),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GrammarNodeKind {
    Root,
    Token(String),
    Alias(String),
    Recursion(String),
    EOI,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct GrammarNode {
    pub kind: GrammarNodeKind,
    pub syntax_name: String,
}

impl Debug for GrammarNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            GrammarNodeKind::Root => f.write_str(&self.syntax_name),
            GrammarNodeKind::Token(token) => {
                f.write_str(&format!("{}({})", self.syntax_name, token))
            }
            GrammarNodeKind::Alias(alias) => f.write_str(&format!("alias({})", alias)),
            GrammarNodeKind::Recursion(alias) => f.write_str(&format!("recursion({})", alias)),
            GrammarNodeKind::EOI => f.write_str(&format!("{}(EOI)", self.syntax_name)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum GrammarPath {
    Optional(Box<GrammarRule>),
    Sequence(LinkedList<GrammarRule>),
    Choice(Vec<GrammarRule>),
}

pub struct GrammarTree {
    pub graph: DiGraph<GrammarNode, ()>,
    pub roots: HashMap<String, NodeIndex>,
}

pub struct GrammarBuilder {
    pub current_rule: String,
    pub rules: HashMap<String, GrammarRule>,
}

impl GrammarBuilder {
    pub fn new() -> Self {
        Self {
            current_rule: "".into(),
            rules: HashMap::new(),
        }
    }

    pub fn add_rule(&mut self, alias: &str, rule: GrammarRule) {
        self.rules.insert(alias.into(), rule);
    }

    pub fn to_digraph(&self) -> GrammarTree {
        let mut graph = DiGraph::new();
        let mut roots = HashMap::new();

        for (alias, rule) in &self.rules {
            let root = graph.add_node(GrammarNode {
                kind: GrammarNodeKind::Root,
                syntax_name: alias.clone(),
            });

            let mut cache = HashMap::new();

            traverse_rule(&mut graph, &mut cache, &self.rules, root, rule, None, false);

            clean(&mut graph, root);
            roots.insert(alias.clone(), root);
        }

        GrammarTree { graph, roots }
    }
}

fn clean(graph: &mut DiGraph<GrammarNode, ()>, node: NodeIndex) {
    let mut children = Vec::new();

    for child in graph.neighbors(node) {
        let weight = graph.node_weight(child).unwrap();
        children.push((child, weight.clone()));
    }

    let mut child_set = HashSet::new();

    for child in children {
        if child_set.contains(&child.1) {
            graph.remove_node(child.0);
        } else {
            child_set.insert(child.1);
            clean(graph, child.0);
        }
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
        // Make sure not to duplicate edges.
        if graph.edges_connecting(parent, node.clone()).count() == 0 {
            graph.add_edge(parent, node.clone(), ());
        }

        if let Some(continuation) = continuation {
            let sequence = GrammarRule::Path(GrammarPath::Sequence(continuation));
            traverse_rule(graph, cache, rules, node.clone(), &sequence, None, false);
        }

        return;
    }

    match &node.as_ref().kind {
        GrammarNodeKind::Token(_) | GrammarNodeKind::EOI => {
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
        GrammarNodeKind::Alias(alias) => {
            let node = graph.add_node(node.as_ref().clone());
            graph.add_edge(parent, node, ());

            let rule = rules.get(alias).unwrap();

            traverse_rule(graph, cache, rules, node, rule, continuation, start_choice);
        }
        GrammarNodeKind::Recursion(_) => {
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
macro_rules! grammar {
    ($($x:expr),*) => {{
        let mut grammar = GrammarBuilder::new();

        macro_rules! token {
            ($token:expr) => {
                GrammarRule::Node(std::rc::Rc::new(GrammarNode {
                    kind: GrammarNodeKind::Token($token.into()),
                    syntax_name: grammar.current_rule.clone(),
                }))
            };
        }

        macro_rules! alias {
            ($alias:expr) => {
                GrammarRule::Node(std::rc::Rc::new(GrammarNode {
                    kind: GrammarNodeKind::Alias($alias.into()),
                    syntax_name: grammar.current_rule.clone(),
                }))
            };
        }

        macro_rules! recursion {
            ($alias:expr) => {
                GrammarRule::Node(std::rc::Rc::new(GrammarNode {
                    kind: GrammarNodeKind::Recursion($alias.into()),
                    syntax_name: grammar.current_rule.clone(),
                }))
            };
        }

        macro_rules! optional {
            ($rule:expr) => {
                GrammarRule::Path(GrammarPath::Optional(Box::new($rule)))
            };
        }

        macro_rules! sequence {
            ($$($$x:expr),*) => {{
                let mut list = std::collections::LinkedList::new();

                $$(
                    list.push_back($$x);
                )+

                GrammarRule::Path(GrammarPath::Sequence(list))
            }};
        }

        macro_rules! choice {
            ($$($$x:expr),*) => {
                GrammarRule::Path(GrammarPath::Choice(vec![$$($$x),*]))
            };
        }

        macro_rules! eoi {
            () => {
                GrammarRule::Node(std::rc::Rc::new(GrammarNode {
                    kind: GrammarNodeKind::EOI,
                    syntax_name: grammar.current_rule.clone(),
                }))
            };
        }

        $(
            grammar.current_rule = $x.0.into();
            grammar.add_rule($x.0.into(), $x.1);
        )+

        grammar
    }};
}
