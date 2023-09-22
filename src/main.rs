use std::{
    cell::RefCell,
    collections::{HashMap, LinkedList},
    rc::Rc,
};

macro_rules! linked_list {
    () => {
        LinkedList::new()
    };
    ($($item:expr),+ $(,)?) => {
        {
            let mut list = LinkedList::new();
            $(
                list.push_back($item);
            )+
            list
        }
    };
}

#[derive(Debug, Clone)]
enum RuleKind {
    Terminal(String),
    Alias(String),
    Recursion(String),
    Sequence(LinkedList<Rule>),
    Choice(Vec<Rule>),
}

#[derive(Debug, Clone)]
struct Rule {
    kind: RuleKind,
    node_name: String,
}

impl Rule {
    fn simplify(&self, grammar: &Grammar) -> (Rule, bool) {
        match &self.kind {
            // Terminals are already simplified.
            RuleKind::Terminal(_) => (self.clone(), true),

            // Aliases simplify to a copy of the rule they are aliasing.
            RuleKind::Alias(alias) => (grammar.rules.get(alias).unwrap().clone(), false),

            // Recursion rules cannot be simplified.
            RuleKind::Recursion(_) => (self.clone(), true),

            // Sequences simplify to a copy where all entries are simplified.
            RuleKind::Sequence(entries) => match entries.len() {
                1 => return entries.front().unwrap().simplify(grammar),
                _ => {
                    let mut simplified = LinkedList::new();
                    let mut is_simplified = true;

                    let mut entries_iter = entries.clone();
                    let mut n = 0;

                    while entries_iter.len() > 0 {
                        let entry = entries_iter.front().unwrap();

                        match &entry.kind {
                            RuleKind::Choice(choices) => {
                                if entries_iter.len() > 1 {
                                    let mut new_choices = Vec::new();

                                    for choice in choices {
                                        let mut sequence = linked_list!(choice.clone());

                                        for entry in entries_iter.clone().split_off(1) {
                                            sequence.push_back(entry);
                                        }

                                        new_choices.push(Rule {
                                            kind: RuleKind::Sequence(sequence),
                                            node_name: self.node_name.clone(),
                                        });
                                    }

                                    let mut sequence = linked_list!();
                                    let mut i = 0;

                                    for entry in entries {
                                        if i >= n {
                                            break;
                                        }

                                        sequence.push_back(entry.clone());

                                        i += 1;
                                    }

                                    sequence.push_back(Rule {
                                        kind: RuleKind::Choice(new_choices),
                                        node_name: self.node_name.clone(),
                                    });

                                    return (
                                        Rule {
                                            kind: RuleKind::Sequence(sequence),
                                            node_name: self.node_name.clone(),
                                        },
                                        false,
                                    );
                                } else {
                                    let (simplified_rule, rule_simplified) =
                                        entry.simplify(grammar);
                                    is_simplified &= rule_simplified;

                                    // Check if the entry is a sequence and flatten it.
                                    if let RuleKind::Sequence(sub_entries) = simplified_rule.kind {
                                        simplified.extend(sub_entries);
                                        is_simplified = false;
                                    } else {
                                        simplified.push_back(simplified_rule);
                                    }
                                }
                            }

                            // If it's not a choice then we just handle it like normal.
                            _ => {
                                let (simplified_rule, rule_simplified) = entry.simplify(grammar);
                                is_simplified &= rule_simplified;

                                // Check if the entry is a sequence and flatten it.
                                if let RuleKind::Sequence(sub_entries) = simplified_rule.kind {
                                    simplified.extend(sub_entries);
                                    is_simplified = false;
                                } else {
                                    simplified.push_back(simplified_rule);
                                }
                            }
                        }

                        entries_iter = entries_iter.split_off(1);
                        n += 1;
                    }

                    (
                        Rule {
                            kind: RuleKind::Sequence(simplified),
                            node_name: self.node_name.clone(),
                        },
                        is_simplified,
                    )
                }
            },

            // Choices simplify to a copy where all choices are simplified.
            RuleKind::Choice(choices) => match choices.len() {
                1 => return choices.first().unwrap().simplify(grammar),
                _ => {
                    let mut simplified = Vec::new();
                    let mut is_simplified = true;

                    for choice in choices {
                        let simplified_rule = choice.simplify(grammar);
                        is_simplified &= simplified_rule.1;
                        simplified.push(simplified_rule.0);
                    }

                    (
                        Rule {
                            kind: RuleKind::Choice(simplified),
                            node_name: self.node_name.clone(),
                        },
                        is_simplified,
                    )
                }
            },
        }
    }

    fn discover_roots(&self, roots: &mut HashMap<String, Rule>, grammar: &Grammar) {
        match &self.kind {
            // Terminals do not have any roots to discover.
            RuleKind::Terminal(_) => {}

            // Alias roots are found by simply traversing the aliased rule.
            RuleKind::Alias(alias) => grammar
                .rules
                .get(alias)
                .unwrap()
                .discover_roots(roots, grammar),

            // Recursion rules are roots. So we add the alias to the roots with a clone of the aliased rule.
            RuleKind::Recursion(alias) => {
                roots.insert(alias.clone(), grammar.rules.get(alias).unwrap().clone());
            }

            // Sequence roots are found by traversing all the rules in the sequence.
            RuleKind::Sequence(entries) => {
                for entry in entries {
                    entry.discover_roots(roots, grammar);
                }
            }

            // Choice roots are found by traversing all the rules in the choice.
            RuleKind::Choice(choices) => {
                for choice in choices {
                    choice.discover_roots(roots, grammar);
                }
            }
        }
    }
}

#[derive(Debug)]
struct Grammar {
    entry: Option<String>,
    rules: HashMap<String, Rule>,
}

impl Grammar {
    fn default() -> Self {
        Self {
            entry: None,
            rules: HashMap::new(),
        }
    }

    fn entry_rule(mut self, rule: Rule) -> Grammar {
        self.entry = Some(rule.node_name.clone());
        self.rules.insert(rule.node_name.clone(), rule);
        self
    }

    fn define_rule(mut self, rule: Rule) -> Grammar {
        self.rules.insert(rule.node_name.clone(), rule);
        self
    }

    fn simplify(self) -> Grammar {
        let entry_name = self.entry.clone().unwrap();
        let entry = self.rules.get(&entry_name).unwrap();
        let mut roots = self.find_root_rules(entry);

        loop {
            let mut simplified = true;
            let mut simplified_roots = HashMap::new();

            for (name, rule) in &roots {
                let simplification = rule.simplify(&self);
                simplified &= simplification.1;
                simplified_roots.insert(name.clone(), simplification.0);
            }

            roots = simplified_roots;

            if simplified {
                break;
            }
        }

        Grammar {
            entry: Some(entry_name),
            rules: roots,
        }
    }

    fn find_root_rules(&self, entry: &Rule) -> HashMap<String, Rule> {
        let mut roots = HashMap::new();

        entry.discover_roots(&mut roots, self);

        roots.insert(entry.node_name.clone(), entry.clone());
        roots
    }
}

#[derive(Debug, Clone)]
enum ParseNodeKind {
    Root,
    Branch,
    Terminal(String),
    NonTerminal(String),
}

#[derive(Debug, Clone)]
struct ParseNode {
    kind: ParseNodeKind,
    node_name: String,
    children: Vec<Rc<RefCell<ParseNode>>>,
}

#[derive(Debug)]
struct ParseTree {
    root: Rc<RefCell<ParseNode>>,
}

impl Grammar {
    fn traverse(&self) -> HashMap<String, ParseTree> {
        let mut parse_trees = HashMap::new();

        for (name, rule) in &self.rules {
            let root = Rc::new(RefCell::new(ParseNode {
                kind: ParseNodeKind::Root,
                node_name: name.clone(),
                children: Vec::new(),
            }));

            Grammar::traverse_rule(rule.clone(), root.clone());

            parse_trees.insert(name.clone(), ParseTree { root });
        }

        parse_trees
    }

    fn traverse_rule(rule: Rule, parent: Rc<RefCell<ParseNode>>) -> Rc<RefCell<ParseNode>> {
        match rule.kind {
            RuleKind::Terminal(token) => {
                let node = Rc::new(RefCell::new(ParseNode {
                    kind: ParseNodeKind::Terminal(token),
                    node_name: rule.node_name,
                    children: Vec::new(),
                }));

                parent.as_ref().borrow_mut().children.push(node.clone());

                node
            }
            RuleKind::Recursion(alias) => {
                let node = Rc::new(RefCell::new(ParseNode {
                    kind: ParseNodeKind::NonTerminal(alias),
                    node_name: rule.node_name,
                    children: Vec::new(),
                }));

                parent.as_ref().borrow_mut().children.push(node.clone());

                node
            }
            RuleKind::Sequence(mut entries) => {
                if entries.len() > 0 {
                    let node = Grammar::traverse_rule(entries.front().unwrap().clone(), parent);

                    let node = Grammar::traverse_rule(
                        Rule {
                            kind: RuleKind::Sequence(entries.split_off(1)),
                            node_name: rule.node_name,
                        },
                        node,
                    );

                    node
                } else {
                    parent
                }
            }
            RuleKind::Choice(choices) => {
                let choice_parent = Rc::new(RefCell::new(ParseNode {
                    kind: ParseNodeKind::Branch,
                    node_name: rule.node_name.clone(),
                    children: Vec::new(),
                }));

                for choice in choices {
                    Grammar::traverse_rule(choice, choice_parent.clone());
                }

                parent.as_ref().borrow_mut().children.push(choice_parent);
                parent
            }
            _ => unreachable!(),
        }
    }
}

fn main() {
    let mut grammar = Grammar::default();
    let mut node_name;

    macro_rules! entry {
        ($node:expr, $rule:expr) => {
            node_name = $node;
            grammar = grammar.entry_rule($rule);
        };
    }

    macro_rules! rule {
        ($node:expr, $rule:expr) => {
            node_name = $node;
            grammar = grammar.define_rule($rule);
        };
    }

    macro_rules! terminal {
        ($token:expr) => {
            Rule {
                kind: RuleKind::Terminal($token.into()),
                node_name: node_name.into(),
            }
        };
    }

    macro_rules! alias {
        ($alias:expr) => {
            Rule {
                kind: RuleKind::Alias($alias.into()),
                node_name: node_name.into(),
            }
        };
    }

    macro_rules! recursion {
        ($alias:expr) => {
            Rule {
                kind: RuleKind::Recursion($alias.into()),
                node_name: node_name.into(),
            }
        };
    }

    macro_rules! sequence {
        ($($x:expr),*) => {
            Rule {
                kind: RuleKind::Sequence(linked_list![$($x),*]),
                node_name: node_name.into(),
            }
        };
    }

    macro_rules! choice {
        ($($x:expr),*) => {
            Rule {
                kind: RuleKind::Choice(vec![$($x),*]),
                node_name: node_name.into(),
            }
        };
    }

    entry!(
        "program",
        choice!(
            alias!("fn_declaration"),
            alias!("struct_declaration"),
            terminal!("EOF")
        )
    );

    rule!(
        "fn_declaration",
        sequence!(
            terminal!("FN"),
            terminal!("IDENT"),
            terminal!("LPAREN"),
            terminal!("RPAREN"),
            terminal!("SEMICOLON")
        )
    );

    rule!(
        "struct_declaration",
        sequence!(
            terminal!("STRUCT"),
            terminal!("IDENT"),
            terminal!("LBRACE"),
            alias!("struct_fields"),
            terminal!("RBRACE")
        )
    );

    rule!(
        "struct_fields",
        choice!(
            alias!("struct_field"),
            sequence!(
                alias!("struct_field"),
                terminal!("COMMA"),
                recursion!("struct_fields")
            )
        )
    );

    rule!(
        "struct_field",
        sequence!(terminal!("IDENT"), terminal!("COLON"), alias!("field_type"))
    );

    rule!(
        "field_type",
        choice!(
            terminal!("IDENT"),
            sequence!(
                terminal!("VEC"),
                terminal!("LT"),
                terminal!("IDENT"),
                terminal!("GT")
            )
        )
    );

    grammar = grammar.simplify();

    std::fs::write("grammar.txt", format!("{:#?}", grammar)).unwrap();
    std::fs::write("parser.txt", format!("{:#?}", grammar.traverse())).unwrap();
}
