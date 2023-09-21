use std::collections::HashMap;

#[derive(Debug, Clone)]
enum RuleKind {
    Terminal(String),
    Alias(String),
    Recursion(String),
    Sequence(Vec<Rule>),
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
                1 => return entries.first().unwrap().simplify(grammar),
                _ => {
                    let mut simplified = Vec::new();
                    let mut is_simplified = true;

                    for entry in entries {
                        let (simplified_rule, rule_simplified) = entry.simplify(grammar);
                        is_simplified &= rule_simplified;

                        // Check if the entry is a sequence and flatten it.
                        if let RuleKind::Sequence(sub_entries) = simplified_rule.kind {
                            simplified.extend(sub_entries);
                            is_simplified = false;
                        } else {
                            simplified.push(simplified_rule);
                        }
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
                kind: RuleKind::Sequence(vec![$($x),*]),
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

    entry!("expr", sequence!(alias!("term"), alias!("expr'")));

    rule!(
        "expr'",
        choice!(
            sequence!(terminal!("PLUS"), alias!("term"), recursion!("expr'")),
            sequence!(terminal!("MINUS"), alias!("term"), recursion!("expr'")),
            terminal!("EOF")
        )
    );

    rule!("term", sequence!(alias!("factor"), alias!("term'")));

    rule!(
        "term'",
        choice!(
            sequence!(terminal!("TIMES"), alias!("factor"), recursion!("term'")),
            sequence!(terminal!("DIVIDE"), alias!("factor"), recursion!("term'")),
            terminal!("EOF")
        )
    );

    rule!(
        "factor",
        choice!(
            sequence!(terminal!("MINUS"), recursion!("factor")),
            alias!("atom")
        )
    );

    rule!(
        "atom",
        choice!(
            terminal!("IDENT"),
            terminal!("NUMBER"),
            sequence!(
                terminal!("L_PAREN"),
                recursion!("expr"),
                terminal!("R_PAREN")
            )
        )
    );

    grammar = grammar.simplify();
    println!("{:#?}", grammar);

    // TODO: Need to traverse all root rules depth first. For each depth we emit each token or recursion that
    // TODO: could possibly be matched at that depth on that branch. We can then use that tree to generate the parser.
}
