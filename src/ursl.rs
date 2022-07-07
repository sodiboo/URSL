use super::*;
use std::fmt::{Display, Formatter, Result};

pub struct InstructionEntry<'a> {
    pub excess_height: usize,
    pub enter_height: usize,
    pub exit_height: Option<usize>,
    pub instruction: Instruction<'a>,
    pub unit: &'a CompilationUnit<'a>,
    pub node: Node<'a>,
}

impl PositionEntry for InstructionEntry<'_> {
    fn pos(&self) -> Position {
        self.node.pos(self.unit)
    }
}

pub enum Instruction<'a> {
    Height(usize),

    Perm(Permutation),
    Const(Literal<'a>),

    In(&'a str),
    Out(&'a str),

    Label(&'a str),
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
            Self::Height(height) => write!(f, "height {height}"),
            Self::Perm(perm) => write!(f, "perm {perm}"),
            Self::Const(lit) => write!(f, "const {lit}"),

            Self::In(port) => write!(f, "in %{port}"),
            Self::Out(port) => write!(f, "out %{port}"),

            Self::Label(label) => write!(f, "label :{label}"),
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
    unit: &'a CompilationUnit<'a>,
) -> Vec<SourceError<'a>> {
    let mut errors = Vec::new();
    let mut height = Some(0usize);
    let mut all_labels = HashMap::<&'a str, usize>::new();
    for inst in nodes {
        // if args.verbose {
        //     println!();
        //     println!("-- parse so far for {func_name}");
        //     for entry in instructions.iter() {
        //         println!("    {}", entry.instruction);
        //     }
        //     if let Some(height) = height {
        //         println!("height = {height}");
        //     } else {
        //         println!("height = ???");
        //     }
        // }
        macro_rules! op {
            () => {
                inst.field("operand", unit)
            };
            (stack) => {
                parse_stack(op!(), unit)
            };
            (num) => {
                parse_num(op!().text(unit))
            };
            (literal) => {
                lower_literal(args, headers, parse_literal(op!(), unit))
            };
            (func) => {
                op!().text(unit)
            };
            (label) => {
                op!().field("name", unit).text(unit)
            };
            (port) => {
                op!().field("name", unit).text(unit)
            };
            (loc) => {{
                let idx = op!(num);
                assert!(
                    idx < locals,
                    "Out of bounds local variable at {}",
                    inst.pos(unit)
                );
                idx
            }};
            (perm) => {
                parse_permutation_sig(op!(), unit).extend_into(&mut errors)
            };
        }

        macro_rules! inst {
            ($height:ident => $e:expr) => {
                {
                    let $height = height.unwrap_or_else(|| panic!("Unknown stack height at {}", inst.pos(unit)));
                    $e
                }
            };

            ($inst:expr; $in:literal -> $out:literal) => { inst!($inst; stack!($in; -> $out)) };
            ($inst:expr; $stack:expr) => {
                inst!(enter_height => {
                    let excess_height = match enter_height.checked_sub($stack.input) {
                        Some(height) => height,
                        None => {
                            err!(errors; unit; inst; 0, "Stack underflow")
                        }
                    };
                    InstructionEntry {
                        excess_height,
                        enter_height,
                        exit_height: Some(excess_height + $stack.output),
                        instruction: $inst,
                        node: inst,
                        unit,
                    }
                })
            }
        }
        let entry = match inst.kind() {
            "height" => {
                let operand = op!(num);
                if let Some(height) = height {
                    if height == operand {
                        // do not put a height instruction when it's nop. emit will assume normalized allocation, which is only okay to do if the height was None
                        continue;
                    } else {
                        // However, if it was different and we're emitting an error, allow the stack to be normalized.
                        // My intutiton says this will probably make more faulty code be slightly less broken with --fuck-it
                        // so yeah, fuck it. allow whatever here. it's not my problem if you intentionally suppress errors
                        err!(errors; unit; inst, "Height directive doesn't match the correct stack height (stack here has height of {height})");
                    }
                }
                InstructionEntry {
                    excess_height: 0,
                    enter_height: operand,
                    exit_height: Some(operand),
                    instruction: Instruction::Height(operand),
                    node: inst,
                    unit,
                }
            }
            "perm" => {
                let operand = op!(perm);
                inst!(Instruction::Perm(operand); stack!(operand.input; -> operand.output.len()))
            }
            "const" => inst!(Instruction::Const(op!(literal)); 0 -> 1),
            "call" => {
                let operand = op!(func);
                let stack = match signatures.get(operand) {
                    Some((stack, _)) => *stack,
                    None => {
                        err!(errors; unit; inst; stack!(0; -> 0), "Call to unknown func {operand}")
                    }
                };
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
            "label" => inst!(Instruction::Label({
                let label = op!(label);
                all_labels.insert(label, instructions.len());
                label
            }); 0 -> 0),
            "jump" => inst!(enter_height => InstructionEntry {
                excess_height: enter_height,
                enter_height,
                exit_height: None,
                instruction: Instruction::Jump(op!(label)),
                node: inst,
                unit,
            }),
            "branch" => inst! { enter_height => {
                let previous = instructions.pop().unwrap_or_else(|| {
                    err!(errors; unit; inst; InstructionEntry {
                        excess_height: enter_height,
                        enter_height,
                        exit_height: Some(enter_height),
                        instruction: Instruction::Call(""),
                        node: inst,
                        unit,
                    }, "Branch without a prefix instruction")
                });
                let opcode = if let Instruction::Call(opcode) = previous.instruction {
                    opcode
                } else {
                    err!(errors; unit; inst; "", "Branch prefix has no branching variant")
                };
                // unwrap should only exist in the above error cases
                // otherwise, it has already been proven to exist by the actual call implementation
                let (stack, branching) = signatures.get(opcode).unwrap_or(&(stack!(0; -> 1), true));
                if !branching {
                    err!(errors; unit; inst, "Branch prefix has no branching variant");
                }
                assert_eq!(stack.output, 1);
                InstructionEntry {
                    enter_height: previous.enter_height,
                    excess_height: previous.excess_height,
                    exit_height: Some(previous.excess_height),
                    instruction: Instruction::Branch(opcode, op!(label)),
                    node: inst,
                    unit,
                }
            }},
            "ret" => inst!(enter_height => {
                if enter_height != returns {
                    err!(errors; unit; inst, "Bad stack height (height here is {enter_height}, but function returns {returns})");
                }
                InstructionEntry {
                    excess_height: 0,
                    enter_height,
                    exit_height: None,
                    instruction: Instruction::Ret,
                    node: inst,
                    unit,
                }
            }),
            "halt" => inst!(enter_height => InstructionEntry {
                    excess_height: enter_height,
                    enter_height,
                    exit_height: None,
                    instruction: Instruction::Halt,
                    node: inst,
                    unit,
            }),
            "custom_instruction" => {
                let opcode = inst.field("opcode", unit).text(unit);
                let (stack, _) = signatures
                    .get(opcode)
                    .unwrap_or_else(|| panic!("Unknown instruction {}", opcode));
                inst!(Instruction::Call(opcode); stack)
            }
            _ => unknown_node(inst, unit),
        };
        height = entry.exit_height;
        instructions.push(entry);
    }
    for entry in instructions.iter() {
        if let Instruction::Jump(label) | Instruction::Branch(_, label) = entry.instruction {
            if let Some(&idx) = all_labels.get(label) {
                let dest = &instructions[idx];
                if entry.excess_height != dest.enter_height {
                    err!(errors; unit; entry.node, "Incorrect stack height on branch (height here is {} but destination expects {})", entry.excess_height, dest.enter_height);
                }
            } else {
                err!(errors; unit; entry.node, "Branch or jump to unknown label {func_name}:{label}")
            }
        }
    }
    if returns == 0 {
        if let Some(height) = height {
            if height != 0 {
                err!(errors; None, "Stack is not empty at the end of {func_name} (height is {height})");
            }
        }
    } else if height.is_some() {
        err!(errors; None, "func {func_name} falls out of its scope without returning, jumping or halting.");
    }
    errors
}

pub fn emit_instructions<'a>(
    args: &Args,
    f: &mut impl Write,
    functions: &BTreeMap<&'a str, Function<'a>>,
    func: &Function<'a>,
    locals: usize,
    instructions: &Vec<InstructionEntry<'a>>,
    max_regs: &mut usize,
) -> io::Result<()> {
    assert!(!instructions.is_empty()); // empty instruction lists are only allowed for -> 0, and parsing normalizes them to end with a ret
    writeln!(f, ".{}", mangle::function_name(func.name))?;
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
            // idx is referring to an arg, skip the ret pointer!
            idx + locals + 1
        } else {
            // idx is referring to a local
            idx - func.stack.input
        }
    };
    let mut reg_alloc = RegisterAllocation::new();
    Ok(for entry in instructions {
        if args.verbose {
            write!(f, "// +{}; {} -> ", entry.excess_height, entry.enter_height)?;
            match entry.exit_height {
                Some(n) => writeln!(f, "{n}")?,
                None => writeln!(f, "?")?,
            }
            println!();
            println!("reg alloc:{reg_alloc:?}");
            println!("emitting: {}", entry.instruction);
        }
        match entry.instruction {
            Instruction::Height(height) => {
                reg_alloc = RegisterAllocation::normal(height);
            }
            Instruction::Ret => {
                reg_alloc.normalize(args, f, max_regs, 0)?;
                if locals != 0 {
                    writeln!(f, "ADD SP SP {locals}")?;
                }
                writeln!(f, "RET")?;
            }
            Instruction::Halt => writeln!(f, "HLT")?,
            Instruction::Const(ref lit) => reg_alloc.push(AllocationSlot::Literal(lit.clone())),
            Instruction::Ref(idx) => {
                writeln!(f, "ADD {} SP {}", reg_alloc.apply_next_reg(), map_loc(idx))?
            }
            Instruction::Get(idx) => {
                writeln!(f, "LLOD {} SP {}", reg_alloc.apply_next_reg(), map_loc(idx))?
            }
            Instruction::Set(idx) => {
                writeln!(f, "LSTR SP {} {}", map_loc(idx), reg_alloc.apply_pop1())?
            }
            Instruction::In(port) => writeln!(f, "IN {} %{port}", reg_alloc.apply_next_reg())?,
            Instruction::Out(port) => writeln!(f, "OUT %{port} {}", reg_alloc.apply_pop1())?,
            Instruction::Label(label) => {
                reg_alloc.normalize(args, f, max_regs, 0)?;
                writeln!(f, ".{}", mangle::local_label(func.name, label))?
            }
            Instruction::Jump(label) => {
                reg_alloc.normalize(args, f, max_regs, 0)?;
                writeln!(f, "JMP .{}", mangle::local_label(func.name, label))?
            }
            Instruction::Branch(prefix, label) => {
                if let Some(Function {
                    body:
                        FunctionBody::Urcl {
                            overloads: _,
                            branch:
                                Some(UrclBranchBody {
                                    input,
                                    instructions,
                                    pos: _,
                                }),
                        },
                    ..
                }) = functions.get(prefix)
                {
                    reg_alloc.normalize(args, f, max_regs, input.len())?;
                    reg_alloc = urcl::emit_instructions(
                        f,
                        instructions,
                        Some((func.name, label)),
                        reg_alloc,
                        input,
                        &Default::default(),
                        max_regs,
                    )?;
                } else {
                    unreachable!("Already checked that func exists.")
                }
            }
            Instruction::Perm(ref perm) => reg_alloc.apply_permutation(perm),
            Instruction::Call(func) => {
                if let Some(func) = functions.get(func) {
                    match &func.body {
                        FunctionBody::Urcl {
                            overloads,
                            branch: _,
                        } => {
                            let (new_reg_alloc, emitted, new_max_regs) = overloads
                                .iter()
                                .map(
                                    |UrclMainBody {
                                         input,
                                         output,
                                         instructions,
                                         pos: _,
                                     }| {
                                        let mut emit = Vec::new();
                                        let mut max_regs = 0;
                                        (
                                            urcl::emit_instructions(
                                                &mut emit,
                                                instructions,
                                                None,
                                                reg_alloc.clone(),
                                                input,
                                                output,
                                                &mut max_regs,
                                            )
                                            .unwrap(),
                                            String::from_utf8(emit).unwrap(),
                                            max_regs,
                                        )
                                    },
                                )
                                .max_by_key(|(_, emit, _)| emit.lines().count())
                                .expect("there should be at least one non-branching overload");
                            reg_alloc = new_reg_alloc;
                            write!(f, "{emitted}")?;
                            *max_regs = new_max_regs;
                        }
                        FunctionBody::Permutation(perm) => reg_alloc.apply_permutation(perm),
                        FunctionBody::Ursl { .. } => {
                            if args.verbose {
                                writeln!(f, "// begin call to {}", func.name)?;
                            }
                            let params = reg_alloc.get(func.stack.input).to_vec();
                            reg_alloc.pop(params.len());
                            let used_regs = reg_alloc.all_used_regs();
                            for i in used_regs.iter().copied() {
                                writeln!(f, "PSH ${i}")?;
                            }
                            if params.len() != 0 {
                                if args.verbose {
                                    writeln!(f, "// push arguments")?
                                }
                                for p in params.iter().rev() {
                                    writeln!(f, "PSH {p}")?
                                }
                            }
                            writeln!(f, "CAL .{}", mangle::function_name(func.name))?;
                            if params.len() != 0 {
                                if args.verbose {
                                    writeln!(f, "// pop arguments")?
                                }
                                writeln!(f, "ADD SP SP {}", params.len())?;
                            }
                            if args.verbose {
                                writeln!(f, "// pop rest of operands")?;
                            }
                            for i in used_regs.into_iter().rev() {
                                writeln!(f, "POP ${}", i + func.stack.output)?
                            }
                            reg_alloc.offset(func.stack.output);
                            for i in 1..=func.stack.output {
                                reg_alloc.push(AllocationSlot::Register(i))
                            }
                            if args.verbose {
                                writeln!(f, "// end call to {}", func.name)?;
                            }
                        }
                    }
                } else {
                    panic!("Already checked that func exists. This should be unreachable.")
                }
            }
            Instruction::IndirectCall(stack) => {
                if args.verbose {
                    writeln!(f, "// begin icall")?;
                }
                let params = reg_alloc.get(stack.input).to_vec();
                reg_alloc.pop(params.len());
                let func = reg_alloc.top();
                reg_alloc.pop(1);
                let used_regs = reg_alloc.all_used_regs();
                for i in used_regs.iter().copied() {
                    writeln!(f, "PSH ${i}")?;
                }
                if params.len() != 0 {
                    if args.verbose {
                        writeln!(f, "// push arguments")?
                    }
                    for p in params.iter().rev() {
                        writeln!(f, "PSH {p}")?
                    }
                }
                writeln!(f, "CAL {func}")?;
                if params.len() != 0 {
                    if args.verbose {
                        writeln!(f, "// pop arguments")?
                    }
                    writeln!(f, "ADD SP SP {}", params.len())?;
                }
                if args.verbose {
                    writeln!(f, "// pop rest of operands")?;
                }
                for i in used_regs.into_iter().rev() {
                    writeln!(f, "POP ${}", i + stack.output)?
                }
                reg_alloc.offset(stack.output);
                for i in 1..=stack.output {
                    reg_alloc.push(AllocationSlot::Register(i))
                }
                if args.verbose {
                    writeln!(f, "// end icall")?;
                }
            }
        }
    })
}
