use super::*;
use std::fmt::{Display, Formatter, Result};

pub struct InstructionEntry<'a> {
    pub instruction: Instruction<'a>,
    pub pos: Position,
}

impl PositionEntry for urcl::InstructionEntry<'_> {
    fn pos(&self) -> &Position {
        &self.pos
    }
}

#[derive(Clone)]
pub enum Instruction<'a> {
    In {
        dest: usize,
        port: &'a str,
    },
    Out {
        port: &'a str,
        source: Source<'a>,
    },
    Jmp {
        dest: BranchDestination<'a>,
    },
    Unary {
        op: &'a str,
        dest: Destination<'a>,
        source: Source<'a>,
    },
    Binary {
        op: &'a str,
        dest: Destination<'a>,
        source1: Source<'a>,
        source2: Source<'a>,
    },
}

impl Display for Instruction<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::In { dest, port } => write!(f, "IN {dest} %{port}"),
            Self::Out { port, source } => write!(f, "IN %{port} {source}"),
            Self::Jmp { dest } => write!(f, "JMP {dest}"),
            Self::Unary { op, dest, source } => write!(f, "{op} {dest} {source}"),
            Self::Binary {
                op,
                dest,
                source1,
                source2,
            } => write!(f, "{op} {dest} {source1} {source2}"),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Destination<'a> {
    Register(usize),
    Branch(BranchDestination<'a>),
}

impl<'a> Destination<'a> {
    pub fn emit(&self, excess_height: usize, branch_target: Option<(&'a str, &'a str)>) -> String {
        match self {
            Self::Register(n) => format!(
                "${}",
                match n {
                    0 => 0,
                    n => excess_height + n,
                }
            ),
            Self::Branch(branch) => branch.emit(branch_target),
        }
    }
}

impl Display for Destination<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.emit(0, None))
    }
}

#[derive(Clone, Copy)]
pub enum BranchDestination<'a> {
    TemporaryLabel(&'a str),
    Relative(isize),
    BranchLabel,
}

impl<'a> BranchDestination<'a> {
    pub fn emit(self, branch_target: Option<(&'a str, &'a str)>) -> String {
        match self {
            Self::BranchLabel => match branch_target {
                Some((func, label)) => format!(".{}", mangle::local_label(func, label)),
                None => format!("{{BRANCH TARGET}}"),
            },
            Self::TemporaryLabel(label) => format!("{{TEMPORARY LABEL :{}}}", label),
            Self::Relative(n) if n >= 0 => format!("~+{n}"),
            Self::Relative(n) => format!("~{n}"),
        }
    }
}

impl Display for BranchDestination<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.emit(None))
    }
}

#[derive(Clone)]
pub enum Source<'a> {
    Register(usize),
    Literal(Literal<'a>),
}

impl<'a> Source<'a> {
    pub fn emit(&self, excess_height: usize) -> String {
        match self {
            Self::Register(n) => format!(
                "${}",
                match n {
                    0 => 0,
                    n => excess_height + n,
                }
            ),
            Self::Literal(lit) => format!("{lit}"),
        }
    }
}

impl Display for Source<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.emit(0))
    }
}

fn parse_source<'a>(args: &Args, headers: &Headers, node: Node, source: &'a str) -> Source<'a> {
    if node.kind() == "register" {
        Source::Register(node.field("idx").text(source).parse().unwrap())
    } else {
        Source::Literal(lower_literal(args, headers, parse_literal(node, source)))
    }
}

pub fn parse_instructions<'a>(
    args: &Args,
    headers: &Headers,
    nodes: Vec<Node<'a>>,
    func_name: &'a str,
    branch_destination: Option<&'a str>,
    source: &'a str,
    cursor: &mut TreeCursor<'a>,
) -> Vec<InstructionEntry<'a>> {
    let mut instructions = Vec::new();
    let mut labels = HashMap::<&'a str, usize>::new();
    for inst in nodes {
        if let Some(label) = parse_label(inst, &labels, &instructions, func_name, source) {
            if let Some(branch_destination) = branch_destination {
                if label == branch_destination {
                    panic!("Duplicate label :{label} previously defined in branch destination, and also at {}", inst.pos());
                }
            }
            labels.insert(label, instructions.len());
        }

        let entry = InstructionEntry {
            pos: inst.pos(),
            instruction: match inst.kind() {
                "jmp" => Instruction::Jmp {
                    dest: BranchDestination::TemporaryLabel(
                        &inst.field("dest").text(source)[1..], // trim :
                    ),
                },
                "urcl_in" => Instruction::In {
                    dest: inst.field("dest").text(source)[1..].parse().unwrap(), // trim $
                    port: &inst.field("source").text(source)[1..],               // trim %
                },
                "urcl_out" => Instruction::Out {
                    port: &inst.field("dest").text(source)[1..], // trim %
                    source: parse_source(args, headers, inst.field("source"), source),
                },
                "urcl_instruction" => {
                    let dest = inst.field("dest");
                    let source_operands = inst
                        .children_by_field_name("source", cursor)
                        .map(|node| parse_source(args, headers, node, source))
                        .collect::<Vec<Source>>();
                    let op = inst.field("op").text(source);
                    let dest = match dest.kind() {
                        "register" => Destination::Register(
                            dest.text(source)[1..].parse().unwrap(), // trim $
                        ),
                        "inst_label" => Destination::Branch(BranchDestination::TemporaryLabel(
                            dest.text(source),
                        )),
                        _ => panic!("Unknown dest node type at {}", dest.pos()),
                    };
                    match &source_operands[..] {
                        [src] => Instruction::Unary {
                            op,
                            dest,
                            source: src.clone(),
                        },
                        [src1, src2] => Instruction::Binary {
                            op,
                            dest,
                            source1: src1.clone(),
                            source2: src2.clone(),
                        },
                        _ => panic!("Unknown source type at {}", inst.pos()),
                    }
                }

                unknown => panic!("Unknown node kind {} at {}", unknown, inst.pos()),
            },
        };
        instructions.push(entry);
    }

    for i in 0..instructions.len() {
        let entry = instructions.get_mut(i).unwrap();
        let lower = |dest| match dest {
            BranchDestination::TemporaryLabel(label) => {
                if let Some(branch) = branch_destination {
                    if label == branch {
                        return BranchDestination::BranchLabel;
                    }
                }
                if let Some(pos) = labels.get(label) {
                    BranchDestination::Relative((*pos as isize) - (i as isize))
                } else {
                    panic!("Unknown label :{label} at {}", entry.pos)
                }
            }
            _ => dest,
        };
        entry.instruction = match entry.instruction.clone() {
            Instruction::Jmp { dest } => Instruction::Jmp { dest: lower(dest) },
            Instruction::Unary {
                op,
                dest: Destination::Branch(dest),
                source,
            } => Instruction::Unary {
                op,
                dest: Destination::Branch(lower(dest)),
                source,
            },
            Instruction::Binary {
                op,
                dest: Destination::Branch(dest),
                source1,
                source2,
            } => Instruction::Binary {
                op,
                dest: Destination::Branch(lower(dest)),
                source1,
                source2,
            },
            other => other,
        }
    }
    instructions
}

pub fn emit_instructions<'a>(
    f: &mut impl Write,
    instructions: &Vec<urcl::InstructionEntry<'a>>,
    branch_target: Option<(&'a str, &'a str)>,
    excess_height: usize,
) -> io::Result<()> {
    Ok(for entry in instructions {
        match &entry.instruction {
            urcl::Instruction::In { dest, port } => writeln!(
                f,
                "IN ${} %{port}",
                match dest {
                    0 => 0,
                    n => excess_height + n,
                }
            )?,
            urcl::Instruction::Out { port, source } => {
                writeln!(f, "OUT %{port} {}", source.emit(excess_height))?
            }
            urcl::Instruction::Jmp { dest } => writeln!(f, "JMP {}", dest.emit(branch_target))?,

            urcl::Instruction::Unary { op, dest, source } => writeln!(
                f,
                "{op} {} {}",
                dest.emit(excess_height, branch_target),
                source.emit(excess_height)
            )?,
            urcl::Instruction::Binary {
                op,
                dest,
                source1,
                source2,
            } => writeln!(
                f,
                "{op} {} {} {}",
                dest.emit(excess_height, branch_target),
                source1.emit(excess_height),
                source2.emit(excess_height)
            )?,
        }
    })
}
