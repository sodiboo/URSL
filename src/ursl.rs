use super::*;
use std::fmt::{Display, Formatter, Result};

pub struct InstructionEntry<'a> {
    pub label: Option<&'a str>,
    pub excess_height: usize,
    pub enter_height: usize,
    pub exit_height: Option<usize>,
    pub instruction: Instruction<'a>,
    pub pos: Position,
}

impl PositionEntry for InstructionEntry<'_> {
    fn pos(&self) -> &Position {
        &self.pos
    }
}

pub enum Instruction<'a> {
    Perm(Permutation),
    Const(Literal<'a>),

    In(&'a str),
    Out(&'a str),

    Jump(&'a str),
    Branch(&'a str, &'a str),

    Halt,

    Call(&'a str),
    IndirectCall(StackBehaviour),
    Ret,

    Ref(usize),
    Get(usize),
    Set(usize),
}

impl Display for Instruction<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Perm(perm) => write!(f, "perm {perm}"),
            Self::Const(lit) => write!(f, "const {lit}"),

            Self::In(port) => write!(f, "in %{port}"),
            Self::Out(port) => write!(f, "out %{port}"),

            Self::Jump(dest) => write!(f, "jump :{dest}"),
            Self::Branch(condition, dest) => write!(f, "{condition} branch :{dest}"),

            Self::Halt => write!(f, "halt"),

            Self::Call(func) => write!(f, "call {func}"),
            Self::IndirectCall(stack) => write!(f, "icall {stack}"),
            Self::Ret => write!(f, "ret"),

            Self::Ref(idx) => write!(f, "ret {idx}"),
            Self::Get(idx) => write!(f, "get {idx}"),
            Self::Set(idx) => write!(f, "set {idx}"),
        }
    }
}

pub fn parse_instructions<'a>(
    args: &Args,
    headers: &Headers,
    signatures: &HashMap<&str, (StackBehaviour, bool)>,
    nodes: Vec<Node<'a>>,
    func_name: &'a str,
    locals: usize,
    returns: usize,
    instructions: &mut Vec<InstructionEntry<'a>>,
    source: &'a str,
    cursor: &mut TreeCursor<'a>,
) {
    let mut height = Some(0usize);
    let mut labels = HashMap::<&'a str, usize>::new();
    // skips {
    for inst in nodes {
        let label = parse_label(inst, &labels, &instructions, func_name, source);

        macro_rules! op {
            () => {
                inst.field("operand")
            };
            (stack) => {
                parse_stack(op!(), source)
            };
            (num) => {
                parse_num(op!().text(source))
            };
            (literal) => {
                lower_literal(args, headers, parse_literal(op!(), source))
            };
            (func) => {
                op!().text(source)
            };
            (label) => {
                &op!().text(source)[1..] // trim :
            };
            (port) => {
                &op!().text(source)[1..] // trim %
            };
            (loc) => {{
                let idx = op!(num);
                assert!(
                    idx < locals,
                    "Out of bounds local variable at {}",
                    inst.pos()
                );
                idx
            }};
            (perm) => {
                parse_permutation_sig(op!(), source, cursor)
            };
        }

        macro_rules! inst {
            ($height:ident => $e:expr) => {
                {
                    let $height = height.unwrap_or_else(|| panic!("Unknown stack height at {}", inst.pos()));
                    $e
                }
            };

            ($inst:expr; $in:literal -> $out:literal) => { inst!($inst; stack!($in; -> $out)) };
            ($inst:expr; $stack:expr) => {
                inst!(enter_height => {
                    let excess_height = enter_height.checked_sub($stack.input).unwrap_or_else(|| panic!("Stack underflow at {}", inst.pos()));
                    InstructionEntry {
                        label,
                        excess_height,
                        enter_height,
                        exit_height: Some(excess_height + $stack.output),
                        instruction: $inst,
                        pos: inst.pos(),
                    }
                })
            }
        }
        let entry = match inst.kind() {
            "height" => {
                assert_eq!(label, None, "Height directive cannot be labeled at {}. Did you mean to put the label *after* it?", inst.pos());
                let operand = op!(num);
                height = match height {
                    Some(height) => {
                        assert_eq!(
                            height,
                            operand,
                            "Height directive at {} doesn't match the correct stack height",
                            inst.pos()
                        );
                        Some(height)
                    }
                    None => Some(operand),
                };
                continue;
            }
            "perm" => {
                let operand = op!(perm);
                inst!(Instruction::Perm(operand); stack!(operand.input; -> operand.output.len()))
            }
            "const" => inst!(Instruction::Const(op!(literal)); 0 -> 1),
            "call" => {
                let operand = op!(func);
                let (stack, _) = signatures.get(operand).unwrap_or_else(|| {
                    panic!("Call to unknown func {} at {}", operand, inst.pos())
                });
                inst!(Instruction::Call(operand); stack)
            }
            "icall" => {
                let stack = op!(stack);
                inst!(Instruction::IndirectCall(stack); stack!(stack.input + 1; -> stack.output))
            }
            "ref" => inst!(Instruction::Ref(op!(loc)); 0 -> 1),
            "get" => inst!(Instruction::Get(op!(loc)); 0 -> 1),
            "set" => inst!(Instruction::Set(op!(loc)); 1 -> 0),

            "in" => inst!(Instruction::In(op!(port)); 0 -> 1),
            "out" => inst!(Instruction::Out(op!(port)); 1 -> 0),
            "jump" => inst!(enter_height => InstructionEntry {
                label,
                excess_height: enter_height,
                enter_height,
                exit_height: None,
                instruction: Instruction::Jump(op!(label)),
                pos: inst.pos(),
            }),
            "branch" => inst! { enter_height => {
                let opcode = inst.field("opcode").text(source);
                if let Some((stack, branching)) = signatures.get(opcode) {
                    if !branching {
                        panic!("Branch following instruction without a branching variant at {}", inst.pos());
                    }
                    assert_eq!(stack.output, 1);
                    let excess_height = enter_height.checked_sub(stack.input).unwrap_or_else(|| panic!("Stack underflow at {}", inst.pos()));
                    InstructionEntry {
                        label,
                        enter_height,
                        excess_height,
                        exit_height: Some(excess_height),
                        instruction: Instruction::Branch(opcode, op!(label)),
                        pos: inst.pos(),
                    }
                } else {
                    panic!("Branch with undefined instruction at {}", inst.pos())
                }
            }},
            "ret" => inst!(enter_height => {
                assert_eq!(enter_height, returns, "Bad stack height (returns {enter_height}, but should return {returns})");
                InstructionEntry {
                    label,
                    excess_height: 0,
                    enter_height,
                    exit_height: None,
                    instruction: Instruction::Ret,
                    pos: inst.pos(),
                }
            }),
            "halt" => inst!(enter_height => InstructionEntry {
                    label,
                    excess_height: enter_height,
                    enter_height,
                    exit_height: None,
                    instruction: Instruction::Halt,
                    pos: inst.pos(),
            }),
            "custom_instruction" => {
                let opcode = inst.field("opcode").text(source);
                let (stack, _) = signatures
                    .get(opcode)
                    .unwrap_or_else(|| panic!("Unknown instruction {}", opcode));
                inst!(Instruction::Call(opcode); stack)
            }
            // Unknown *node kind*
            unknown => panic!(
                "Invalid node type `{}` (expected some kind of instruction)",
                unknown
            ),
        };
        if let Some(label) = label {
            labels.insert(label, instructions.len());
        }
        height = entry.exit_height;
        instructions.push(entry);
    }
    for entry in instructions.iter() {
        if let Instruction::Jump(label) | Instruction::Branch(_, label) = entry.instruction {
            let dest = &instructions[*labels.get(label).unwrap_or_else(|| {
                panic!("Branch or jump to unknown label {}:{}", func_name, label)
            })];
            assert_eq!(
                entry.excess_height, dest.enter_height,
                "Incorrect stack height on branch (got {} but destination has {})",
                entry.excess_height, dest.enter_height,
            );
        }
    }
    if returns == 0 {
        if let Some(height) = height {
            assert_eq!(
                height, 0,
                "Stack is not empty at the end of {func_name} (height is {height})",
            );
            instructions.push(InstructionEntry {
                label: None,
                excess_height: 0,
                enter_height: 0,
                exit_height: None,
                instruction: Instruction::Ret,
                pos: Default::default(),
            });
        }
    } else if height.is_some() {
        panic!("func {func_name} falls out of its scope without returning, jumping or halting.",);
    }
}

pub fn emit_instructions<'a>(
    args: &Args,
    f: &mut impl Write,
    functions: &BTreeMap<&'a str, Function<'a>>,
    func: &Function<'a>,
    locals: usize,
    instructions: &Vec<InstructionEntry<'a>>,
) -> io::Result<()> {
    assert!(!instructions.is_empty()); // empty instruction lists are only allowed for -> 0, and parsing normalizes them to end with a ret
    let name = func.name.trim1('$');
    writeln!(f, ".{}", mangle::function_name(name))?;
    if args.garbage_initialized_locals {
        if func.stack.input != 0 {
            writeln!(f, "SUB SP SP {locals}")?;
        }
    } else {
        for _ in 0..locals {
            writeln!(f, "PSH 0")?;
        }
    }
    let map_loc = |idx| {
        // this is really just (idx - func.stack.input) % (func.stack.input + locals)
        // but i think this is cleaner?
        // this is required because, for function pointers to work, the function needs to allocate its own locals
        // and with that, the locals are at the bottom, so that it doesn't have to move arguments to make space for locals
        // but i didn't wanna change behaviour since i quite like `get 0` being arg 0
        // however, the actual output still needs to bend around the new memory layout
        // which is why this closure exists
        if idx < func.stack.input {
            // idx is referring to an arg
            idx + locals
        } else {
            // idx is referring to a local
            idx - func.stack.input
        }
    };
    Ok(for entry in instructions {
        if args.verbose {
            write!(f, "// +{}; {} -> ", entry.excess_height, entry.enter_height)?;
            match entry.exit_height {
                Some(n) => writeln!(f, "{n}")?,
                None => writeln!(f, "?")?,
            }
        }
        if let Some(label) = entry.label {
            writeln!(f, ".{}", mangle::local_label(name, label))?;
        }
        let excess_height = entry.excess_height;
        let r1 = excess_height + 1;
        match entry.instruction {
            Instruction::Ret => {
                writeln!(f, "ADD SP SP {}", func.stack.input + locals)?;
                writeln!(f, "RET")?;
            }
            Instruction::Halt => writeln!(f, "HLT")?,
            Instruction::Const(ref lit) => writeln!(f, "IMM ${r1} {lit}")?,
            Instruction::Ref(idx) => writeln!(f, "ADD ${r1} SP {}", map_loc(idx))?,
            Instruction::Get(idx) => writeln!(f, "LLOD ${r1} SP {}", map_loc(idx))?,
            Instruction::Set(idx) => writeln!(f, "LSTR SP {} ${r1}", map_loc(idx))?,
            Instruction::In(port) => writeln!(f, "IN ${r1} %{port}")?,
            Instruction::Out(port) => writeln!(f, "OUT %{port} ${r1}")?,
            Instruction::Jump(label) => writeln!(f, "JMP .{}", mangle::local_label(name, label))?,
            Instruction::Branch(prefix, label) => {
                if let Some(Function {
                    body:
                        FunctionBody::Urcl {
                            branch: Some(ref branch),
                            ..
                        },
                    ..
                }) = functions.get(prefix)
                {
                    urcl::emit_instructions(f, branch, Some((name, label)), excess_height)?;
                } else {
                    unreachable!("Already checked that func exists.")
                }
            }
            Instruction::Perm(ref perm) => {
                urcl::emit_instructions(
                    f,
                    &compile_permutation(perm, entry.pos),
                    None,
                    excess_height,
                )?;
            }
            Instruction::Call(func) => {
                if let Some(func) = functions.get(func) {
                    match func.body {
                        FunctionBody::Urcl {
                            ref instructions, ..
                        } => urcl::emit_instructions(f, instructions, None, excess_height)?,
                        FunctionBody::Ursl { .. } => {
                            if args.verbose {
                                writeln!(f, "// begin call {}", func.name)?;
                            }
                            for i in 1..=excess_height {
                                writeln!(f, "PSH ${i}")?;
                            }
                            writeln!(
                                f,
                                "PSH ~+{}", // return pointer
                                // 2 because of the jump, and we want the inst *after* that
                                2 + func.stack.input
                            )?;
                            for i in 1..=func.stack.input {
                                writeln!(f, "PSH ${}", excess_height + i)?;
                            }
                            writeln!(f, "JMP .{}", mangle::function_name(func.name.trim1('$')))?;

                            if args.verbose {
                                writeln!(f, "// return from {}", func.name)?;
                            }
                            if excess_height != 0 {
                                for i in 1..=func.stack.output {
                                    writeln!(f, "MOV ${} ${i}", i + excess_height)?;
                                }
                                for i in (1..=excess_height).rev() {
                                    writeln!(f, "POP ${i}")?;
                                }
                            }
                        }
                    }
                } else {
                    panic!("Already checked that func exists. This should be unreachable.")
                }
            }
            Instruction::IndirectCall(stack) => {
                let func = excess_height + 1;
                if args.verbose {
                    writeln!(f, "// begin icall {stack}")?;
                }
                for i in 1..=excess_height {
                    writeln!(f, "PSH ${i}")?;
                }
                writeln!(
                    f,
                    "PSH ~+{}", // return pointer
                    // 2 because of the jump, and we want the inst *after* that
                    2 + stack.input
                )?;
                for i in 1..=stack.input {
                    writeln!(f, "PSH ${}", func + i)?;
                }
                writeln!(f, "JMP ${func}")?;

                if args.verbose {
                    writeln!(f, "// return from icall")?;
                }
                if excess_height != 0 {
                    for i in 1..=stack.output {
                        writeln!(f, "MOV ${} ${i}", i + excess_height)?;
                    }
                    for i in (1..=excess_height).rev() {
                        writeln!(f, "POP ${i}")?;
                    }
                }
            }
        }
    })
}
