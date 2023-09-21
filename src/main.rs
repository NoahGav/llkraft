use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Rule {
    Root(Box<Rule>),
    Terminal(String),
    Alias(String),
    Recursion(String),
    Sequence(Vec<Rule>),
    Choice(Vec<Rule>),
}

macro_rules! root {
    ($root:expr) => {
        Rule::Root(Box::new($root))
    };
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
    fn simplify(&self, grammar: &Grammar) -> Rule {
        match self {
            // Root rules are not directly simplified.
            Rule::Root(_) => unreachable!(),

            // Terminals are already simplified.
            Rule::Terminal(_) => self.clone(),

            // Aliases simplify to a copy of the rule they are aliasing.
            Rule::Alias(alias) => grammar.rules.get(alias).unwrap().clone(),

            // Recursion rules cannot be simplified.
            Rule::Recursion(_) => self.clone(),

            // Sequences simplify to a copy where all entries are simplified.
            Rule::Sequence(entries) => match entries.len() {
                1 => return entries.first().unwrap().simplify(grammar),
                _ => {
                    let mut simplified = Vec::new();

                    for entry in entries {
                        simplified.push(entry.simplify(grammar));
                    }

                    Rule::Sequence(simplified)
                }
            },

            // Choices simplify to a copy where all choices are simplified.
            Rule::Choice(choices) => match choices.len() {
                1 => return choices.first().unwrap().simplify(grammar),
                _ => {
                    let mut simplified = Vec::new();

                    for choice in choices {
                        simplified.push(choice.simplify(grammar));
                    }

                    Rule::Choice(simplified)
                }
            },
        }
    }

    fn discover_roots(&self, roots: &mut HashMap<String, Rule>, grammar: &Grammar) {
        match self {
            // We don't directly search for roots from a root rule.
            Rule::Root(_) => unreachable!(),

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
    rules: HashMap<String, Rule>,
}

impl Grammar {
    fn default() -> Self {
        Self {
            rules: HashMap::new(),
        }
    }

    fn define_rule(mut self, name: &str, rule: Rule) -> Grammar {
        self.rules.insert(name.into(), rule);
        self
    }

    fn simplify(self) {
        let entry = self.find_entry_rule();
        let roots = self.find_root_rules(entry);

        // TODO: Now fully simplify all roots.

        println!("{:?}", roots);
    }

    fn find_entry_rule(&self) -> (String, &Rule) {
        for (name, rule) in &self.rules {
            match rule {
                Rule::Root(rule) => return (name.clone(), rule),
                _ => {}
            }
        }

        unreachable!(
            "No entry rule defined. Please define a singular entry rule using the 'root!(...)' macro."
        )
    }

    fn find_root_rules(&self, entry: (String, &Rule)) -> HashMap<String, Rule> {
        let mut roots = HashMap::new();

        entry.1.discover_roots(&mut roots, self);

        roots.insert(entry.0, entry.1.clone());
        roots
    }
}

fn main() {
    let grammar = Grammar::default()
        .define_rule("program", root!(alias!("expr")))
        .define_rule(
            "expr",
            sequence!(
                alias!("term"),
                terminal!("PLUS"),
                choice!(recursion!("expr"), terminal!("EOF"))
            ),
        )
        .define_rule("term", terminal!("IDENT"));

    grammar.simplify();
}
