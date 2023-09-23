#[macro_export]
macro_rules! terminal {
    ($token:expr) => {
        GrammarRule::Node(GrammarNode::Terminal($token.into()))
    };
}

#[macro_export]
macro_rules! alias {
    ($alias:expr) => {
        GrammarRule::Node(GrammarNode::Alias($alias.into()))
    };
}

#[macro_export]
macro_rules! leaf {
    ($alias:expr) => {
        GrammarRule::Node(GrammarNode::Leaf($alias.into()))
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
