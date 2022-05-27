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
            write!(f, "${} ", i + 1)?;
        }
        write!(f, "] -> [ ")?;
        for i in &self.output {
            write!(f, "${} ", i + 1)?;
        }
        write!(f, "]")
    }
}

pub fn parse_permutation_sig<'a>(
    node: Node<'a>,
    source: &str,
    cursor: &mut TreeCursor<'a>,
) -> Permutation {
    assert_eq!(node.kind(), "permutation");
    let cursor2 = &mut cursor.clone();
    parse_permutation_common(
        node.field("input")
            .children_by_field_name("items", cursor)
            .map(|n| n.text(source)),
        move |inputs, i, name| {
            if let Some(&(other, _)) = inputs.get(name) {
                panic!(
                    "Duplicate identifier in permutation input at {other} and {}",
                    node.pos()
                );
            } else {
                (name, (node.pos(), i))
            }
        },
        node.field("output")
            .children_by_field_name("items", cursor2)
            .map(|node| node.text(source)),
        move |inputs, name| {
            if let Some(&(_, index)) = inputs.get(name) {
                index
            } else {
                panic!("Unknown identifier in permutation output at {}", node.pos());
            }
        },
    )
}

pub fn parse_permutation(input: Vec<&str>, output: Vec<&str>) -> Permutation {
    parse_permutation_common(
        input.into_iter(),
        move |_, i, name| (name, i),
        output.into_iter(),
        move |inputs, name| inputs[name],
    )
}

fn parse_permutation_common<'a, K: Eq + Hash, T>(
    input: impl Iterator<Item = K>,
    map_input: impl Fn(&HashMap<K, T>, usize, K) -> (K, T),
    output: impl Iterator<Item = &'a str>,
    map_output: impl Fn(&HashMap<K, T>, &'a str) -> usize,
) -> Permutation {
    let mut names = HashMap::new();
    for (i, ident) in input.enumerate() {
        let (k, v) = map_input(&names, i, ident);
        names.insert(k, v);
    }
    Permutation {
        input: names.len(),
        output: output.map(move |name| map_output(&names, name)).collect(),
    }
}

pub fn compile_permutation<'a>(
    perm: &Permutation,
    pos: Position,
) -> Vec<urcl::InstructionEntry<'a>> {
    let mut movs = Vec::new();
    let mut changes = perm
        .output
        .iter()
        .enumerate()
        // src is the value, dest is the index, enumerate gives (i, val)
        // swap here for consistency, even though it could totally be filter_map
        .map(|(dest, &src)| (src, dest))
        .filter(|(src, dest)| (src != dest))
        .collect::<Vec<_>>();
    // Changes that are written to a register that no other changes will read from. They are safe to do immediately.
    while let Some(dangling) = changes
        .iter()
        .position(|&(_, dest)| !changes.iter().any(|&(src, _)| src == dest))
    {
        movs.push(changes.swap_remove(dangling));
    }
    // Circular references are the only ones left, so a temporary register is needed
    let temp = perm.output.len();
    while let Some((first_src, mut last_dest)) = changes.pop() {
        let mut circular = vec![temp, first_src];
        while let Some(i) = changes.iter().position(|&(src, _)| src == last_dest) {
            let (_, dest) = changes.swap_remove(i);
            circular.push(last_dest);
            last_dest = dest;
        }
        circular.push(temp);

        for i in 1..circular.len() {
            movs.push((circular[i], circular[i - 1]));
        }
    }
    movs.into_iter()
        .map(|(src, dest)| urcl::InstructionEntry {
            instruction: urcl::Instruction::Unary {
                op: "MOV",
                dest: urcl::Destination::Register(1 + dest),
                source: urcl::Source::Register(1 + src),
            },
            pos,
        })
        .collect()
}
