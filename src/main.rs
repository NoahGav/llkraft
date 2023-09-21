use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Rule {
    Terminal(String),
    Alias(String),
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

macro_rules! sequence {
    ($x:expr, $($more:expr)*) => {
        Rule::Sequence(vec![$x, $($more), *])
    };
}

macro_rules! choice {
    ($x:expr, $($more:expr)*) => {
        Rule::Choice(vec![$x, $($more), *])
    };
}

impl Rule {
    fn simplify(&self, grammar: &Grammar) -> Rule {
        match self {
            // Terminals are already simplified.
            Rule::Terminal(_) => self.clone(),

            // Aliases simplify to a copy of the rule they are aliasing.
            Rule::Alias(alias) => grammar.rules.get(alias).unwrap().clone(),

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

    fn simplify(self) -> (Grammar, bool) {
        let program = self.rules.get("program").unwrap().simplify(&self);

        let mut grammar = Grammar::default();
        let unsimplified = grammar.traverse(&program, &self);
        grammar = grammar.define_rule("program", program);
        (grammar, unsimplified)
    }

    fn traverse(&mut self, rule: &Rule, old: &Grammar) -> bool {
        let mut unsimplified = false;

        match rule {
            Rule::Terminal(_) => {}
            Rule::Alias(alias) => {
                let aliased = old.rules.get(alias).unwrap().clone();

                self.traverse(&aliased, old);
                self.rules.insert(alias.clone(), aliased);

                unsimplified = true;
            }
            Rule::Sequence(entries) => {
                for entry in entries {
                    unsimplified |= self.traverse(entry, old);
                }
            }
            Rule::Choice(choices) => {
                for choice in choices {
                    unsimplified |= self.traverse(choice, old);
                }
            }
        }

        unsimplified
    }
}

fn main() {
    let mut grammar = Grammar::default()
        .define_rule("program", choice!(alias!("decl"), terminal!("EOF")))
        .define_rule(
            "decl",
            choice!(
                sequence!(terminal!("EXPORT"), alias!("decl`")),
                alias!("decl`")
            ),
        )
        .define_rule("decl`", choice!(alias!("fn"), alias!("struct")))
        .define_rule("fn", terminal!("FN"))
        .define_rule("struct", terminal!("STRUCT"));

    loop {
        let result = grammar.simplify();
        grammar = result.0;

        if !result.1 {
            break;
        }
    }

    grammar = grammar.simplify().0;
    println!("{:#?}", grammar);
}
