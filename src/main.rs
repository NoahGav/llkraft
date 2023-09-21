use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Rule {
    Terminal(String),
    Alias(String),
    Recursion(String),
    Sequence(Vec<Rule>),
    Choice(Vec<Rule>),
}

macro_rules! terminal {
    ($token:expr) => {
        Rule::Terminal($token.into())
    };
}

macro_rules! alias {
    ($alias:expr) => {
        Rule::Alias($alias.into())
    };
}

macro_rules! recursion {
    ($alias:expr) => {
        Rule::Recursion($alias.into())
    };
}

macro_rules! sequence {
    ($($x:expr),*) => {
        Rule::Sequence(vec![$($x),*])
    };
}

macro_rules! choice {
    ($($x:expr),*) => {
        Rule::Choice(vec![$($x),*])
    };
}

impl Rule {
    fn simplify(&self, grammar: &Grammar) -> (Rule, bool) {
        match self {
            // Terminals are already simplified.
            Rule::Terminal(_) => (self.clone(), true),

            // Aliases simplify to a copy of the rule they are aliasing.
            Rule::Alias(alias) => (grammar.rules.get(alias).unwrap().clone(), false),

            // Recursion rules cannot be simplified.
            Rule::Recursion(_) => (self.clone(), true),

            // Sequences simplify to a copy where all entries are simplified.
            Rule::Sequence(entries) => match entries.len() {
                1 => return entries.first().unwrap().simplify(grammar),
                _ => {
                    let mut simplified = Vec::new();
                    let mut is_simplified = true;

                    for entry in entries {
                        let simplified_rule = entry.simplify(grammar);
                        is_simplified &= simplified_rule.1;
                        simplified.push(simplified_rule.0);
                    }

                    (Rule::Sequence(simplified), is_simplified)
                }
            },

            // Choices simplify to a copy where all choices are simplified.
            Rule::Choice(choices) => match choices.len() {
                1 => return choices.first().unwrap().simplify(grammar),
                _ => {
                    let mut simplified = Vec::new();
                    let mut is_simplified = true;

                    for choice in choices {
                        let simplified_rule = choice.simplify(grammar);
                        is_simplified &= simplified_rule.1;
                        simplified.push(simplified_rule.0);
                    }

                    (Rule::Choice(simplified), is_simplified)
                }
            },
        }
    }

    fn discover_roots(&self, roots: &mut HashMap<String, Rule>, grammar: &Grammar) {
        match self {
            // Terminals do not have any roots to discover.
            Rule::Terminal(_) => {}

            // Alias roots are found by simply traversing the aliased rule.
            Rule::Alias(alias) => grammar
                .rules
                .get(alias)
                .unwrap()
                .discover_roots(roots, grammar),

            // Recursion rules are roots. So we add the alias to the roots with a clone of the aliased rule.
            Rule::Recursion(alias) => {
                roots.insert(alias.clone(), grammar.rules.get(alias).unwrap().clone());
            }

            // Sequence roots are found by traversing all the rules in the sequence.
            Rule::Sequence(entries) => {
                for entry in entries {
                    entry.discover_roots(roots, grammar);
                }
            }

            // Choice roots are found by traversing all the rules in the choice.
            Rule::Choice(choices) => {
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

    fn entry_rule(mut self, name: &str, rule: Rule) -> Grammar {
        self.entry = Some(name.into());
        self.rules.insert(name.into(), rule);
        self
    }

    fn define_rule(mut self, name: &str, rule: Rule) -> Grammar {
        self.rules.insert(name.into(), rule);
        self
    }

    fn simplify(self) -> Grammar {
        let entry_name = self.entry.clone().unwrap();
        let entry = self.rules.get(&entry_name).unwrap();
        let mut roots = self.find_root_rules((entry_name.clone(), entry.clone()));

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

    fn find_root_rules(&self, entry: (String, Rule)) -> HashMap<String, Rule> {
        let mut roots = HashMap::new();

        entry.1.discover_roots(&mut roots, self);

        roots.insert(entry.0.clone(), entry.1.clone());
        roots
    }
}

fn main() {
    let mut grammar = Grammar::default()
        .entry_rule("program", alias!("expr"))
        .define_rule(
            "expr",
            sequence!(
                alias!("term"),
                terminal!("PLUS"),
                choice!(recursion!("expr"), terminal!("EOF"))
            ),
        )
        .define_rule("term", terminal!("IDENT"));

    grammar = grammar.simplify();
    println!("{:#?}", grammar);
}
