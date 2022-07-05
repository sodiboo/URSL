use super::*;
use std::fmt::{self, Display, Formatter, Result};

pub struct InstructionEntry<'a> {
    pub instruction: Instruction<'a>,
    pub pos: Position<'a>,
}

#[derive(Clone)]
pub enum Instruction<'a> {
    In {
        dest: Register<'a>,
        port: &'a str,
    },
    Out {
        port: &'a str,
        source: Source<'a>,
    },
    Jmp {
        dest: BranchDestination<'a>,
    },
    Generic {
        op: &'a str,
        dest: Destination<'a>,
        sources: Vec<Source<'a>>,
    },
}

impl Display for Instruction<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::In { dest, port } => write!(f, "IN {dest} %{port}"),
            Self::Out { port, source } => write!(f, "IN %{port} {source}"),
            Self::Jmp { dest } => write!(f, "JMP {dest}"),
            Self::Generic { op, dest, sources } => sources
                .iter()
                .fold(write!(f, "{op} {dest}"), |result, source| {
                    result.and_then(|()| write!(f, " {source}"))
                }),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Register<'a> {
    Index(usize),
    Named(&'a str),
    Input(&'a str),
}

impl Display for Register<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Index(idx) => write!(f, "${idx}"),
            Self::Named(name) => write!(f, "&{name}"),
            Self::Input(name) => write!(f, "<{name}>"),
        }
    }
}

pub struct StackBindings<'a>(Vec<Register<'a>>);

impl StackBindings<'_> {
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl Display for StackBindings<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0.iter().fold(Ok(()), |result, reg| {
            result.and_then(|()| write!(f, " {reg}"))
        })
    }
}

impl Default for StackBindings<'_> {
    fn default() -> Self {
        Self(vec![])
    }
}

pub fn parse_stack_bindings<'a>(
    nodes: impl Iterator<Item = Node<'a>>,
    unit: &'a CompilationUnit<'a>,
) -> StackBindings<'a> {
    StackBindings(nodes.map(|node| parse_register(node, unit)).collect())
}

#[derive(Clone, Copy)]
pub enum Destination<'a> {
    Register(Register<'a>),
    Branch(BranchDestination<'a>),
}

impl Display for Destination<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Register(reg) => write!(f, "{reg}"),
            Self::Branch(dest) => write!(f, "{dest}"),
        }
    }
}

#[derive(Clone, Copy)]
pub enum BranchDestination<'a> {
    TemporaryLabel(Option<&'a str>),
    Relative(isize),
    BranchLabel,
}

impl Display for BranchDestination<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::BranchLabel => write!(f, "{{BRANCH TARGET}}"),
            Self::TemporaryLabel(label) => {
                write!(f, "{{TEMPORARY LABEL :{}}}", label.unwrap_or("$"))
            }
            Self::Relative(n) if *n >= 0 => write!(f, "~+{n}"),
            Self::Relative(n) => write!(f, "~{n}"),
        }
    }
}

#[derive(Clone)]
pub enum Source<'a> {
    Register(Register<'a>),
    Literal(Literal<'a>),
}
impl Display for Source<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Register(reg) => write!(f, "{reg}"),
            Self::Literal(lit) => write!(f, "{lit}"),
        }
    }
}

fn parse_source<'a>(
    args: &Args,
    headers: &Headers,
    node: Node<'a>,
    unit: &'a CompilationUnit<'a>,
) -> Source<'a> {
    match node.kind() {
        "index_register" | "named_register" | "input_register" => {
            Source::Register(parse_register(node, unit))
        }
        _ => Source::Literal(lower_literal(args, headers, parse_literal(node, unit))),
    }
}

fn parse_register<'a>(node: Node<'a>, unit: &'a CompilationUnit<'a>) -> Register<'a> {
    match node.kind() {
        "index_register" => Register::Index(node.field("index", unit).text(unit).parse().unwrap()),
        "named_register" => Register::Named(node.field("name", unit).text(unit)),
        "input_register" => Register::Input(node.field("name", unit).text(unit)),
        _ => unknown_node(node, unit),
    }
}

pub fn parse_instructions<'a>(
    args: &Args,
    headers: &Headers,
    node: Node<'a>,
    func_name: &'a str,
    branch_destination: Option<&'a str>,
    unit: &'a CompilationUnit<'a>,
) -> (Vec<InstructionEntry<'a>>, Vec<SourceError<'a>>) {
    let mut errors = Vec::new();
    let mut instructions = Vec::<InstructionEntry<'a>>::new();
    let mut labels = HashMap::<&'a str, usize>::new();
    for inst in node
        .children_by_field_name("instruction", &mut unit.tree.walk())
        .collect::<Vec<_>>()
    {
        for label in
            inst.children_by_field_name("label", &mut unit.tree.walk())
                .map(|node| {
                    let label = &node.field("name", unit).text(unit)[1..]; // trim :
                    if let Some(idx) = labels.get(label) {
                        err!(errors; None, "Duplicate label {func_name}:{label} (previously defined at {})", instructions[*idx].pos);
                    }
                    label
                }).collect::<Vec<_>>() // collect so errors is not borrowed twice
        {
            if let Some(branch_destination) = branch_destination {
                if label == branch_destination {
                    err!(errors; unit; inst, "Duplicate label :{label} previously defined in the branch clause destination parameter");
                }
            }
            labels.insert(label, instructions.len());
        }

        let inst = inst.field("instruction", unit);

        let entry = InstructionEntry {
            pos: inst.pos(unit),
            instruction: match inst.kind() {
                "urcl_jmp" => {
                    let dest = inst.field("dest", unit);
                    Instruction::Jmp {
                        dest: BranchDestination::TemporaryLabel(parse_label_ref(dest, unit)),
                    }
                }
                "urcl_in" => Instruction::In {
                    dest: parse_register(inst.field("dest", unit), unit),
                    port: inst.field("source", unit).field("name", unit).text(unit),
                },
                "urcl_out" => Instruction::Out {
                    port: &inst.field("dest", unit).field("name", unit).text(unit),
                    source: parse_source(args, headers, inst.field("source", unit), unit),
                },
                "urcl_generic" => Instruction::Generic {
                    op: inst.field("op", unit).text(unit),
                    dest: {
                        let dest = inst.field("dest", unit);
                        if let "inst_label" | "end_label" = dest.kind() {
                            Destination::Branch(BranchDestination::TemporaryLabel(parse_label_ref(
                                dest, unit,
                            )))
                        } else {
                            Destination::Register(parse_register(dest, unit))
                        }
                    },
                    sources: {
                        let sources = inst
                            .children_by_field_name("source", &mut unit.tree.walk())
                            .map(|node| parse_source(args, headers, node, unit))
                            .collect::<Vec<Source>>();
                        if sources.is_empty() {
                            err!(errors; unit; inst, "No source operands; expected at least one??");
                        }
                        sources
                    },
                },
                _ => unknown_node(inst, unit),
            },
        };
        instructions.push(entry);
    }

    let end = instructions.len() as isize;
    for i in 0..instructions.len() {
        let entry = instructions.get_mut(i).unwrap();
        let lower = |dest| match dest {
            BranchDestination::TemporaryLabel(label) => {
                if let Some(label) = label {
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
                } else {
                    // end label
                    BranchDestination::Relative(end - (i as isize))
                }
            }
            _ => dest,
        };
        entry.instruction = match entry.instruction.clone() {
            Instruction::Jmp { dest } => Instruction::Jmp { dest: lower(dest) },
            Instruction::Generic {
                op,
                dest: Destination::Branch(dest),
                sources,
            } => Instruction::Generic {
                op,
                dest: Destination::Branch(lower(dest)),
                sources,
            },
            other => other,
        }
    }
    (instructions, errors)
}

fn parse_label_ref<'a>(node: Node<'a>, unit: &'a CompilationUnit<'a>) -> Option<&'a str> {
    match node.kind() {
        "inst_label" => Some(node.field("name", unit).text(unit)),
        "end_label" => None,
        _ => unknown_node(node, unit),
    }
}

pub fn emit_instructions<'a>(
    f: &mut impl Write,
    instructions: &Vec<urcl::InstructionEntry<'a>>,
    branch_target: Option<(&'a str, &'a str)>,
    reg_alloc: RegisterAllocation,
    StackBindings(input): &StackBindings<'a>,
    StackBindings(output): &StackBindings<'a>,
    max_regs: &mut usize,
) -> io::Result<RegisterAllocation> {
    fn emit_dest(&dest: &BranchDestination, branch_target: Option<(&str, &str)>) -> String {
        match dest {
            BranchDestination::TemporaryLabel(_) => {
                unreachable!("Temporary label should have been lowered already.")
            }
            BranchDestination::Relative(n) if n < 0 => format!("~{n}"),
            BranchDestination::Relative(n) => format!("~+{n}"),
            BranchDestination::BranchLabel => match branch_target {
                Some((func, label)) => {
                    let mut label = mangle::local_label(func, label);
                    label.insert(0, '.');
                    label
                }
                None => unreachable!("Branch to external label is missing a label."),
            },
        }
    }

    let mut regs = HashMap::new();
    regs.insert(Register::Index(0), 0);

    {
        let input_regs = reg_alloc.get(input.len());
        for i in 0..input.len() {
            let input = input[i];
            if let Register::Input(_) = input {
                regs.entry(input).or_insert_with(|| input_regs[i]);
            } else if let Register::Index(0) = input {
                // discard input; no-op
            } else if reg_alloc.is_unique(input_regs[i]) {
                assert_eq!(regs.insert(input, input_regs[i]), None);
            } else {
                let next = reg_alloc.next();
                writeln!(f, "MOV ${next} ${}", input_regs[i])?;
                regs.insert(input, next);
            }
        }
    }

    for &reg in output.iter() {
        regs.entry(reg).or_insert_with(|| reg_alloc.next());
    }

    for entry in instructions {
        match &entry.instruction {
            urcl::Instruction::In { dest, port } => writeln!(
                f,
                "IN ${} %{port}",
                regs.entry(*dest).or_insert_with(|| reg_alloc.next()),
            )?,
            urcl::Instruction::Out { port, source } => {
                write!(f, "OUT %{port}")?;
                match source {
                    Source::Literal(lit) => writeln!(f, " {lit}"),
                    Source::Register(reg) => writeln!(
                        f,
                        " ${}",
                        regs.entry(*reg).or_insert_with(|| reg_alloc.next())
                    ),
                }?
            }
            urcl::Instruction::Jmp { dest } => {
                writeln!(f, "JMP {}", emit_dest(dest, branch_target))?
            }

            urcl::Instruction::Generic { op, dest, sources } => {
                write!(f, "{op}")?;
                match dest {
                    Destination::Register(reg) => write!(
                        f,
                        " ${}",
                        regs.entry(*reg).or_insert_with(|| reg_alloc.next())
                    )?,
                    Destination::Branch(dest) => write!(f, " {}", emit_dest(dest, branch_target))?,
                }
                for source in sources {
                    match source {
                        Source::Literal(lit) => write!(f, " {lit}"),
                        Source::Register(reg) => write!(
                            f,
                            " ${}",
                            regs.entry(*reg).or_insert_with(|| reg_alloc.next())
                        ),
                    }?
                }
                writeln!(f)?;
            }
        }
    }

    let mut reg_alloc = reg_alloc;
    reg_alloc.pop(input.len());
    for reg in output {
        reg_alloc.push(regs[&reg]);
    }
    for (_, reg) in regs {
        *max_regs = (*max_regs).max(reg);
    }
    Ok(reg_alloc)
}
