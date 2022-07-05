use super::*;
use std::{
    fmt::{Display, Formatter, Result},
    hash::Hash,
};

#[derive(PartialEq, Eq)]
pub struct Permutation {
    pub input: usize,
    pub output: Vec<usize>,
}

impl Display for Permutation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "[ ")?;
        for i in 0..self.input {
            write!(f, "{} ", i + 1)?;
        }
        write!(f, "] -> [ ")?;
        for i in &self.output {
            write!(f, "{} ", i + 1)?;
        }
        write!(f, "]")
    }
}

pub fn parse_permutation_sig<'a>(
    node: Node<'a>,
    unit: &'a CompilationUnit<'a>,
) -> (Permutation, Vec<SourceError<'a>>) {
    assert_eq!(node.kind(), "permutation");
    let mut errors_inputs = Vec::new();
    let mut errors_outputs = Vec::new();
    let perm = parse_permutation_common(
        node.field("input", unit)
            .children_by_field_name("items", &mut unit.tree.walk())
            .map(|n| n.text(unit)),
        |inputs, i, name| {
            if let Some(old) = inputs.remove(name) {
                err!(errors_inputs; unit; node; (name, old), "Duplicate identifier in permutation input: {name}")
            } else {
                (name, (node.pos(unit), i))
            }
        },
        node.field("output", unit)
            .children_by_field_name("items", &mut unit.tree.walk()),
        |inputs, node| {
            if let Some(&(_, index)) = inputs.get(node.text(unit)) {
                index
            } else {
                err!(errors_outputs; unit; node; 0, "Unknown identifier in permutation output")
            }
        },
    );
    let mut errors = Vec::new();
    errors.extend(errors_inputs);
    errors.extend(errors_outputs);
    (perm, errors)
}

pub fn parse_permutation(input: Vec<&str>, output: Vec<&str>) -> Permutation {
    parse_permutation_common(
        input.into_iter(),
        move |_, i, name| (name, i),
        output.into_iter(),
        move |inputs, name| inputs[name],
    )
}

fn parse_permutation_common<'a, K: Eq + Hash, T, V>(
    input: impl Iterator<Item = K>,
    mut map_input: impl FnMut(&mut HashMap<K, T>, usize, K) -> (K, T),
    output: impl Iterator<Item = V>,
    mut map_output: impl FnMut(&HashMap<K, T>, V) -> usize,
) -> Permutation {
    let mut names = HashMap::new();
    for (i, ident) in input.enumerate() {
        let (k, v) = map_input(&mut names, i, ident);
        names.insert(k, v);
    }
    Permutation {
        input: names.len(),
        output: output.map(move |name| map_output(&names, name)).collect(),
    }
}
