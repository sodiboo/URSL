mod defs;
mod exts;

use defs::*;
use exts::*;

use clap::Parser;
use std::{
    collections::{BTreeMap, HashMap},
    fs::{self, File},
    io::{self, Write},
};
use tree_sitter::{Node, TreeCursor};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long = "input-file")]
    input: String,

    #[clap(short, long = "output-file")]
    output: String,

    /// Parses escape sequences and emits the exact codepoint in the output. This will break some URCL compilers since the URCL code can then contain null bytes and newlines in char literals.
    #[clap(short = 'c', long)]
    emit_chars_literally: bool,

    /// Emits char literals as numeric literals corresponding to their char codes. This should work with all URCL compilers. Both -c or -C are optional, and without either, char literals are left alone. URCL does not officially support char literals, which is why these are useful
    #[clap(short = 'C', long)]
    emit_chars_as_numbers: bool,

    /// Print lowering of code before translation to URCL, and include additional details in comments in code output.
    #[clap(short, long)]
    verbose: bool,

    /// Allocates locals in bulk by subtracting the desired amount from the stack pointer. By default, they are overwritten to be zero when used. Bulk allocation will be somewhat faster, especially with many locals, but changes code behaviour. This is an optimization that is only safe when your code definitely assigns locals before reading them.
    #[clap(long)]
    garbage_initialized_locals: bool,

    /// Removes predefined instructions. With this parameter, only the following instructions are predefined: const, in, out, jump, branch, halt, call, ret, stack, get, set
    #[clap(short, long)]
    minimal: bool,
}

fn parse_literal<'a>(node: Node, source: &'a str) -> Literal<'a> {
    match node.kind() {
        "char_literal" => {
            let char_literal_value = node.field("value");
            match char_literal_value.kind() {
                "char" => {
                    let char_text = char_literal_value.text(source);
                    assert_eq!(char_text.len(), 1);
                    let ch = char_text.chars().nth(0).unwrap();
                    Literal::Char(ch)
                }
                "char_escape" => {
                    let char_text = char_literal_value.text(source);
                    assert_eq!(char_text.len(), 2);
                    let ch = char_text.chars().nth(1).unwrap();
                    Literal::CharEscape(ch)
                }
                _ => {
                    panic!("Invalid value for char_literal, maybe the source file contained an error or extra node? Error at {}", node.pos());
                }
            }
        }
        "number" => Literal::Num(parse_num(node.text(source))),
        "macro" => Literal::Macro(&node.text(source)[1..]), // trim @
        "data_label" => Literal::Label(&node.text(source)[1..]), // trim .
        "function_name" => Literal::Func(&node.text(source)[1..]), // trim $
        "mem" => Literal::Mem(node.text(source)[1..].parse::<u64>().unwrap()), // trim #
        _ => {
            panic!("Invalid value for literal, maybe the source file contained an error or extra node? Error at {}", node.pos());
        }
    }
}

fn parse_urcl_source<'a>(args: &Args, node: Node, source: &'a str) -> UrclSource<'a> {
    if node.kind() == "register" {
        UrclSource::Register(node.field("idx").text(source).parse::<u64>().unwrap())
    } else {
        UrclSource::Literal(lower_literal(args, parse_literal(node, source)))
    }
}

fn parse_num(text: &str) -> u64 {
    if text.starts_with("0x") {
        let digits = &text[2..];
        u64::from_str_radix(digits, 16).unwrap()
    } else if text.starts_with("0b") {
        let digits = &text[2..];
        u64::from_str_radix(digits, 2).unwrap()
    } else if text.starts_with("0o") {
        let digits = &text[2..];
        u64::from_str_radix(digits, 8).unwrap()
    } else {
        text.parse::<u64>().unwrap()
    }
}

// these functions ensure all labels in a program are unique. these can be parsed back into the original data they were mangled from, which isn't actually necessary, but is an intuitive proof that they'll all be unique
fn mangle_data_label(label: &str) -> String {
    format!("data_{}", label)
}

fn mangle_local_label(function: &str, label: &str) -> String {
    format!(
        "label_{}_{}_{}_{}",
        function.len(),
        label.len(),
        function,
        label
    )
}

fn mangle_function_name(function: &str) -> String {
    format!("func_{}", function)
}

fn lower_literal<'a>(args: &Args, element: Literal<'a>) -> Literal<'a> {
    let mut element = element;
    if args.emit_chars_literally {
        element = if let Literal::CharEscape(escape) = element {
            Literal::Char(match escape {
                '\'' => '\'',
                '\\' => '\\',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '0' => '\0',
                _ => panic!("Unknown escape sequence (unreachable)"),
            })
        } else {
            element
        }
    }
    if args.emit_chars_as_numbers {
        element = if let Literal::Char(ch) = element {
            Literal::Num(ch as u64)
        } else {
            element
        }
    }
    element
}

fn main() -> io::Result<()> {
    let args = &{
        let mut a = Args::parse();
        if a.emit_chars_as_numbers {
            a.emit_chars_literally = true;
        }
        a
    };
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(tree_sitter_ursl::language()).unwrap();
    let source = fs::read_to_string(&args.input).unwrap();
    let tree = parser.parse(&source, None).unwrap();
    let mut cursor = tree.root_node().walk();
    let mut dummy_cursor = cursor.clone();
    let defs = {
        let mut defs = Vec::<(&str, DefValue)>::new();
        if !cursor.down() {
            panic!("File parsed to an empty syntax tree. Am i a joke to you?");
        }
        while cursor.node().kind() == "definition" {
            let label = &cursor.node().field("label").text(&source)[1..]; // trim .
            let value_node = cursor.node().field("value");
            let value: DefValue = match value_node.kind() {
                "array" => {
                    assert_eq!(value_node.kind(), "array");
                    DefValue::Array(
                        value_node
                            .named_children(&mut dummy_cursor)
                            .filter(|node| node.kind() != "comment")
                            .map(|node| lower_literal(&args, parse_literal(node, &source)))
                            .collect(),
                    )
                }
                _ => DefValue::Single(lower_literal(args, parse_literal(value_node, &source))),
            };
            defs.push((label, value));
            if !cursor.next() {
                panic!("No functions")
            }
        }
        defs
    };

    if args.verbose {
        for (label, val) in &defs {
            println!(".{label} {val}");
        }
    }

    let functions = parse_functions(args, &mut cursor, &source);

    if let Some(Function { stack, .. }) = functions.get("$main") {
        assert_eq!(stack.input, 0, "$main may not take any arguments");
        assert_eq!(stack.output, 0, "$main may not return any values");
    } else {
        panic!("No $main function")
    };

    let mut f = File::create(&args.output)?;

    writeln!(f, "PSH ~+2")?;
    writeln!(f, "JMP .{}", mangle_function_name("main"))?;
    writeln!(f, "HLT")?;

    for (label, val) in defs {
        writeln!(f, ".{} {val}", mangle_data_label(label))?;
    }

    for func in functions.values() {
        if let FunctionBody::Ursl {
            locals,
            ref instructions,
        } = func.body
        {
            assert!(!instructions.is_empty()); // empty instruction lists are only allowed for -> 0, and parsing normalizes them to end with a ret
            let func_name = &func.name[1..]; // trim $ prefix
            writeln!(f, ".{}", mangle_function_name(func_name))?;
            if args.garbage_initialized_locals {
                if func.stack.input != 0 {
                    writeln!(f, "SUB SP SP {locals}")?;
                }
            } else {
                for _ in 0..locals {
                    writeln!(f, "PSH 0")?;
                }
            }
            recursive_write_function(
                args,
                &mut f,
                func_name,
                func,
                locals,
                HashMap::new(),
                0,
                &functions,
                instructions,
            )?;
        }
    }

    Ok(())
}

fn recursive_write_function<'a>(
    args: &Args,
    f: &mut File,
    func_name: &'a str,
    func: &Function<'a>,
    locals: u64,
    bindings: HashMap<&'a str, u64>,
    extra_height: u64,
    functions: &BTreeMap<&'a str, Function<'a>>,
    instructions: &Vec<InstructionEntry<'a>>,
) -> io::Result<()> {
    Ok(for entry in instructions {
        if args.verbose {
            write!(f, "// +{}; {} -> ", entry.excess_height, entry.enter_height)?;
            match entry.exit_height {
                Some(n) => writeln!(f, "{n}")?,
                None => writeln!(f, "?")?,
            }
        }
        if let Some(label) = entry.label {
            writeln!(f, ".{}", mangle_local_label(func_name, label))?;
        }
        let excess_height = entry.excess_height + extra_height;
        let r1 = excess_height + 1;
        match entry.instruction {
            Instruction::Ret => {
                if extra_height != 0 {
                    for i in 1..=func.stack.output {
                        writeln!(f, "MOV ${i} ${}", extra_height + i)?;
                    }
                }
                if locals != 0 {
                    writeln!(f, "ADD SP SP {}", locals)?;
                }
                writeln!(f, "RET")?;
            }
            Instruction::Halt => writeln!(f, "HLT")?,
            Instruction::Const(ref lit) => writeln!(f, "IMM ${r1} {lit}")?,
            Instruction::Stack(idx) => writeln!(f, "ADD ${r1} SP {idx}")?,
            Instruction::Get(name) => writeln!(f, "MOV ${r1} ${}", bindings[name])?,
            Instruction::Set(name) => writeln!(f, "MOV ${} ${r1}", bindings[name])?,
            Instruction::In(port) => writeln!(f, "IN ${r1} %{port}")?,
            Instruction::Out(port) => writeln!(f, "OUT %{port} ${r1}")?,
            Instruction::Jump(label) => {
                writeln!(f, "JMP .{}", mangle_local_label(func_name, label))?
            }
            Instruction::Branch(prefix, label) => {
                if let Some(Function {
                    body:
                        FunctionBody::Urcl {
                            branch: Some(branch),
                            ..
                        },
                    ..
                }) = functions.get(prefix)
                {
                    emit_inline_inst(
                        f,
                        branch,
                        Some(&mangle_local_label(func_name, label)),
                        excess_height,
                    )?;
                } else {
                    panic!("Already checked that func exists. This should be unreachable.")
                }
            }
            Instruction::Call(func) => {
                if let Some(func) = functions.get(func) {
                    match func.body {
                        FunctionBody::Urcl {
                            ref instructions, ..
                        } => emit_inline_inst(f, instructions, None, excess_height)?,
                        FunctionBody::Ursl { .. } => {
                            if args.verbose {
                                writeln!(f, "// begin call {}", func.name)?;
                            }
                            for i in 1..=excess_height {
                                writeln!(f, "PSH ${i}")?;
                            }
                            for i in 1..=func.stack.input {
                                writeln!(f, "MOV ${i} ${}", excess_height + i)?;
                            }
                            writeln!(f, "PSH ~+2")?; // return pointer
                            writeln!(f, "JMP .{}", mangle_function_name(&func.name[1..]))?; // trim $ prefix
                            if args.verbose {
                                writeln!(f, "// return from {}", func.name)?;
                            }
                            if excess_height != 0 {
                                for i in 1..=func.stack.output {
                                    writeln!(f, "MOV ${} ${i}", excess_height + i)?;
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
                if args.verbose {
                    writeln!(f, "// begin icall {stack}")?;
                }
                if excess_height != 0 {
                    for i in 1..=excess_height {
                        writeln!(f, "PSH ${i}")?;
                    }
                    for i in 1..=stack.input {
                        writeln!(f, "MOV ${i} ${}", excess_height + i)?;
                    }
                }
                writeln!(f, "PSH ~+2")?; // return pointer
                writeln!(f, "JMP ${}", excess_height + stack.input + 1)?;

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
            Instruction::Let(ref new_bindings, ref insts) => {
                let mut bindings = bindings.clone();
                for i in 0..new_bindings.len() {
                    bindings.insert(new_bindings[i], excess_height + i as u64 + 1);
                }
                let extra_height = excess_height + new_bindings.len() as u64;
                recursive_write_function(
                    args,
                    f,
                    func_name,
                    func,
                    locals,
                    bindings,
                    extra_height,
                    &functions,
                    insts,
                )?;
                for i in 0..new_bindings.len() {
                    writeln!(
                        f,
                        "MOV ${} ${}",
                        excess_height + i as u64,
                        extra_height + i as u64
                    )?;
                }
            }
        }
    })
}

fn emit_inline_inst<'a>(
    f: &mut File,
    instructions: &Vec<UrclInstructionEntry<'a>>,
    branch_target: Option<&'a str>,
    excess_height: u64,
) -> io::Result<()> {
    Ok(for entry in instructions {
        match &entry.instruction {
            UrclInstruction::In { dest, port } => writeln!(
                f,
                "IN ${} %{port}",
                match dest {
                    0 => 0,
                    n => excess_height + n,
                }
            )?,
            UrclInstruction::Out { port, source } => {
                writeln!(f, "OUT %{port} {}", source.emit(excess_height))?
            }
            UrclInstruction::Jmp { dest } => writeln!(f, "JMP {}", dest.emit(branch_target))?,

            UrclInstruction::Unary { op, dest, source } => writeln!(
                f,
                "{op} {} {}",
                dest.emit(excess_height, branch_target),
                source.emit(excess_height)
            )?,
            UrclInstruction::Binary {
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

fn parse_stack_sig(node: Node, source: &str) -> StackBehaviour {
    match node.child_by_field_name("stack") {
        Some(node) => parse_stack(node, source),
        None => stack!(0; -> 0),
    }
}

fn parse_stack(node: Node, source: &str) -> StackBehaviour {
    StackBehaviour {
        input: parse_num(node.field("params").text(source)),
        output: parse_num(node.field("returns").text(source)),
    }
}

fn parse_locals(node: Node, source: &str) -> u64 {
    match node.child_by_field_name("locals") {
        Some(node) => parse_num(node.text(source)),
        None => 0,
    }
}

fn parse_label<'a>(
    inst: Node,
    labels_pos: &HashMap<&'a str, Position>,
    func_name: &'a str,
    source: &'a str,
) -> Option<&'a str> {
    match inst.child_by_field_name("label") {
        Some(node) => {
            let label = Some(&node.text(source)[1..]); // trim :
            if let Some(label) = &label {
                if let Some(pos) = labels_pos.get(label) {
                    panic!(
                        "Duplicate label {}:{} at {} and {}",
                        func_name,
                        label,
                        pos,
                        node.pos()
                    );
                }
            }
            label
        }
        None => None,
    }
}

fn parse_functions<'a>(
    args: &Args,
    cursor: &mut TreeCursor<'a>,
    source: &'a str,
) -> BTreeMap<&'a str, Function<'a>> {
    // btreemap ensures deterministic ordering when writing output
    let mut functions = BTreeMap::<&'a str, Function<'a>>::new();
    let mut signatures = HashMap::<&'a str, (StackBehaviour, bool)>::new();
    let mut nodes = HashMap::<&'a str, Node<'a>>::new();

    if !args.minimal {
        std_instructions(&mut functions, &mut signatures);
    }

    loop {
        let node = cursor.node();
        match node.kind() {
            "func" => {
                let stack = parse_stack_sig(node, source);
                let locals = parse_locals(node, source);
                let name = node.field("name").text(source); // don't trim $, that way it doesn't collide with insts
                functions.insert(
                    name,
                    Function {
                        name: name,
                        stack,
                        body: FunctionBody::Ursl {
                            locals,
                            instructions: Vec::new(),
                        },
                        pos: node.pos(),
                    },
                );
                signatures.insert(name, (stack, false));
                nodes.insert(name, node.field("instructions"));
            }
            "inst" => {
                let stack = parse_stack_sig(node, source);
                let name = node.field("name").text(source);
                if ["halt", "ret"].contains(&name) {
                    panic!("inst {name} at {} is also defined as intrinsic", node.pos());
                }
                if let Some(f) = functions.get(&name) {
                    panic!("inst {name} at {} is also defined at {}", node.pos(), f.pos);
                }
                let instructions =
                    parse_urcl_instructions(args, &node.field("instructions"), name, source, None);
                let branch = node.child_by_field_name("branch").map(|branch_clause| {
                    parse_urcl_instructions(
                        args,
                        &branch_clause.field("instructions"),
                        name,
                        source,
                        Some(&branch_clause.field("label").text(source)[1..]), // trim :
                    )
                });
                let is_branch = branch.is_some();
                if is_branch && stack.output != 1 {
                    panic!(
                        "inst {name} has a branching variant, but does not return 1 value! at {}",
                        node.pos()
                    )
                }
                functions.insert(
                    name,
                    Function {
                        name: name,
                        stack,
                        body: FunctionBody::Urcl {
                            instructions,
                            branch,
                        },
                        pos: node.pos(),
                    },
                );
                signatures.insert(name, (stack, is_branch));
            }
            _ => panic!(
                "Unknown node kind, expected func or inline. Error at {}",
                node.pos()
            ),
        }
        if !cursor.next() {
            break;
        }
    }

    for func in functions.values_mut() {
        match &mut func.body {
            FunctionBody::Ursl {
                locals,
                instructions,
            } => {
                let locals = *locals;
                let node = nodes.get(func.name).unwrap();
                *instructions = parse_instructions(
                    args,
                    &signatures,
                    node,
                    func.name,
                    func.stack,
                    locals,
                    source,
                );
                if args.verbose {
                    println!("func {} : {} + {locals} {{", func.name, func.stack);
                    for entry in instructions {
                        if let Some(label) = &entry.label {
                            println!(" :{label}")
                        }
                        println!("  {}", entry.instruction);
                        match entry.instruction {
                            Instruction::Ret
                            | Instruction::Halt
                            | Instruction::Jump(_)
                            | Instruction::Branch(_, _) => println!(),
                            _ => (),
                        }
                    }
                    println!("}}\n");
                }
            }
            FunctionBody::Urcl {
                instructions,
                branch,
            } => {
                if args.verbose {
                    println!("inst {} : {} {{", func.name, func.stack);
                    for entry in instructions {
                        println!("  {}", entry.instruction)
                    }
                    if let Some(branch) = branch {
                        println!("}} branch {{");
                        for entry in branch {
                            println!("  {}", entry.instruction)
                        }
                    }
                    println!("}}")
                }
            }
        }
    }
    functions
}

fn parse_instructions<'a>(
    args: &Args,
    signatures: &HashMap<&str, (StackBehaviour, bool)>,
    node: &Node,
    func_name: &'a str,
    stack: StackBehaviour,
    locals: u64,
    source: &'a str,
) -> Vec<InstructionEntry<'a>> {
    assert_eq!(node.kind(), "instruction_list");
    let mut labels_pos = HashMap::<&'a str, Position>::new();
    let mut labels = HashMap::<&'a str, Vec<u64>>::new();
    let (mut instructions, height) = parse_instructions_raw(
        args,
        signatures,
        node,
        func_name,
        stack.input,
        locals,
        stack.output,
        &Vec::new(),
        &Vec::new(),
        &mut labels_pos,
        &mut labels,
        source,
    );
    verify_labels(0, func_name, &labels, &instructions, &instructions);
    if stack.input == 0 {
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
                pos: node.end_pos(),
            });
        }
    } else {
        assert!(
            height.is_none(),
            "func {} falls out of its scope without returning, jumping or halting.",
            func_name,
        );
    }
    instructions
}

fn verify_labels<'a>(
    extra_height: u64,
    func_name: &'a str,
    labels: &HashMap<&'a str, Vec<u64>>,
    top_instructions: &Vec<InstructionEntry<'a>>,
    instructions: &Vec<InstructionEntry<'a>>,
) {
    for entry in instructions {
        match entry.instruction {
            Instruction::Branch(_, label) | Instruction::Jump(label) => {
                if let Some(indices) = labels.get(label) {
                    let height = find_label_height(0, indices.as_slice(), top_instructions);
                    assert_eq!(
                        entry.excess_height, height,
                        "Incorrect stack height on branch (got {} but destination has {height})",
                        entry.excess_height,
                    );
                } else {
                    panic!("Branch or jump to unknown label {}:{}", func_name, label)
                }
            }
            Instruction::Let(_, ref insts) => {
                verify_labels(
                    extra_height + entry.excess_height,
                    func_name,
                    labels,
                    top_instructions,
                    insts,
                );
            }
            _ => (),
        }
    }
}

fn find_label_height<'a>(
    extra_height: u64,
    indices: &[u64],
    instructions: &Vec<InstructionEntry<'a>>,
) -> u64 {
    assert!(!indices.is_empty());
    let entry = &instructions[indices[0] as usize];
    let indices = &indices[1..];
    if indices.is_empty() {
        extra_height + entry.enter_height
    } else if let Instruction::Let(_, ref insts) = entry.instruction {
        find_label_height(extra_height + entry.excess_height, indices, insts)
    } else {
        panic!("Non-empty label indices at non-compound instruction. This should be unreachable");
    }
}

fn parse_instructions_raw<'a>(
    args: &Args,
    signatures: &HashMap<&str, (StackBehaviour, bool)>,
    node: &Node,
    func_name: &'a str,
    start_height: u64,
    locals: u64,
    returns: u64,
    indices: &Vec<u64>,
    bindings: &Vec<&'a str>,
    labels_pos: &mut HashMap<&'a str, Position>,
    labels: &mut HashMap<&'a str, Vec<u64>>,
    source: &'a str,
) -> (Vec<InstructionEntry<'a>>, Option<u64>) {
    assert_eq!(node.kind(), "instruction_list");
    let mut cursor = node.walk();
    let mut height = Some(start_height);
    let mut instructions = Vec::<InstructionEntry<'a>>::new();
    cursor.down();
    // skips {
    while cursor.next() {
        let inst = cursor.node();
        if inst.kind() == "}" {
            break;
        }
        let label = parse_label(inst, &labels_pos, func_name, source);

        if let Some(label) = label {
            let mut indices = indices.clone();
            indices.push(instructions.len() as u64);
            labels_pos.insert(label, inst.pos());
            labels.insert(label, indices);
        }

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
                lower_literal(args, parse_literal(op!(), source))
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
            (var) => {{
                let var = op!().text(source);
                assert!(
                    bindings.contains(&var),
                    "Reference to unknown binding {var} at {}",
                    inst.pos()
                );
                var
            }};
        }

        macro_rules! height {
            (check) => {
                height.unwrap_or_else(|| panic!("Unknown stack height at {}", inst.pos()))
            };
            ($height:ident - $e:expr) => {
                $height
                    .checked_sub($e)
                    .unwrap_or_else(|| panic!("Stack underflow at {}", inst.pos()))
            };
        }

        macro_rules! inst {
            ($height:ident => $e:expr) => {
                {
                    let $height = height!(check);
                    $e
                }
            };

            ($inst:expr; $in:literal -> $out:literal) => { inst!($inst; stack!($in; -> $out)) };
            ($inst:expr; $stack:expr) => {
                inst!(enter_height => {
                    let excess_height = height!(enter_height - $stack.input);
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
            "stack" => inst!(Instruction::Stack(op!(loc)); 0 -> 1),
            "let" => {
                let enter_height = height!(check);
                let names: Vec<&'a str> = inst
                    .children_by_field_name("names", &mut cursor)
                    .map(|node| node.text(source))
                    .collect();
                cursor.reset(inst);
                let excess_height = height!(enter_height - names.len() as u64);
                let mut indices = indices.clone();
                indices.push(instructions.len() as u64);
                let mut bindings = bindings.clone();
                bindings.extend(names.iter());
                let (insts, exit) = parse_instructions_raw(
                    args,
                    signatures,
                    &inst.field("instructions"),
                    func_name,
                    0,
                    locals,
                    returns,
                    &indices,
                    &bindings,
                    labels_pos,
                    labels,
                    source,
                );
                InstructionEntry {
                    label,
                    excess_height,
                    enter_height,
                    exit_height: exit.map(|h| excess_height + h),
                    instruction: Instruction::Let(names, insts),
                    pos: inst.pos(),
                }
            }
            "get" => inst!(Instruction::Get(op!(var)); 0 -> 1),
            "set" => inst!(Instruction::Set(op!(var)); 1 -> 0),

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
            // This is matching a node kind. All of the above are special in the grammar, because of operands, and their node kind matches the instruction.
            // This one is just called "instruction" because it's all zero-operand instructions, including custom instructions
            // Really all of these ought to be nested matches, but i don't think that's possible with tree-sitter
            "instruction" => match inst.field("opcode").text(source) {
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

                inst => {
                    let (stack, _) = signatures
                        .get(inst)
                        .unwrap_or_else(|| panic!("Unknown instruction {}", inst));
                    inst!(Instruction::Call(inst); stack)
                }
            },
            // Unknown *node kind*
            unknown => panic!(
                "Invalid node type at {} (expected some kind of instruction)",
                unknown
            ),
        };
        height = entry.exit_height;
        instructions.push(entry);
    }
    (instructions, height)
}

fn parse_urcl_instructions<'a>(
    args: &Args,
    node: &Node,
    func_name: &'a str,
    source: &'a str,
    branch_destination: Option<&'a str>,
) -> Vec<UrclInstructionEntry<'a>> {
    let mut cursor = node.walk();
    let mut dummy_cursor = node.walk();

    let mut instructions = Vec::<UrclInstructionEntry>::new();
    let mut labels_pos = HashMap::<&'a str, Position>::new();
    let mut labels = HashMap::<&'a str, usize>::new();

    let reg = |node: Node| node.field("idx").text(source).parse::<u64>().unwrap();

    cursor.down(); // to "{"
    while cursor.next() {
        let inst = cursor.node();
        if inst.kind() == "}" {
            break;
        }

        if let Some(label) = parse_label(inst, &labels_pos, func_name, source) {
            if let Some(branch_destination) = branch_destination {
                if label == branch_destination {
                    panic!("Duplicate label :{label} previously defined in branch destination, and also at {}", inst.pos());
                }
            }
            labels.insert(label, instructions.len());
            labels_pos.insert(label, inst.pos());
        }

        let entry = UrclInstructionEntry {
            pos: inst.pos(),
            instruction: match inst.kind() {
                "jmp" => UrclInstruction::Jmp {
                    dest: UrclBranchDestination::TemporaryLabel(
                        &inst.field("dest").text(source)[1..], // trim :
                    ),
                },
                "urcl_in" => UrclInstruction::In {
                    dest: reg(inst.field("dest")),
                    port: &inst.field("source").text(source)[1..], // trim %
                },
                "urcl_out" => UrclInstruction::Out {
                    port: &inst.field("dest").text(source)[1..], // trim %
                    source: parse_urcl_source(args, inst.field("source"), source),
                },
                "urcl_instruction" => {
                    let dest = inst.field("dest");
                    let source_operands = inst
                        .children_by_field_name("source", &mut dummy_cursor)
                        .map(|node| parse_urcl_source(args, node, source))
                        .collect::<Vec<UrclSource>>();
                    let op = inst.field("op").text(source);
                    let dest = match dest.kind() {
                        "register" => UrclDestination::Register(
                            dest.field("idx").text(source).parse::<u64>().unwrap(),
                        ),
                        "inst_label" => UrclDestination::Branch(
                            UrclBranchDestination::TemporaryLabel(dest.text(source)),
                        ),
                        _ => panic!("Unknown dest node type at {}", dest.pos()),
                    };
                    match &source_operands[..] {
                        [src] => UrclInstruction::Unary {
                            op,
                            dest,
                            source: src.clone(),
                        },
                        [src1, src2] => UrclInstruction::Binary {
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
            UrclBranchDestination::TemporaryLabel(label) => {
                if let Some(branch) = branch_destination {
                    if label == branch {
                        return UrclBranchDestination::BranchLabel;
                    }
                }
                if let Some(pos) = labels.get(label) {
                    UrclBranchDestination::Relative((*pos as i64) - (i as i64))
                } else {
                    panic!("Unknown label :{label} at {}", entry.pos)
                }
            }
            _ => dest,
        };
        entry.instruction = match entry.instruction.clone() {
            UrclInstruction::Jmp { dest } => UrclInstruction::Jmp { dest: lower(dest) },
            UrclInstruction::Unary {
                op,
                dest: UrclDestination::Branch(dest),
                source,
            } => UrclInstruction::Unary {
                op,
                dest: UrclDestination::Branch(lower(dest)),
                source,
            },
            UrclInstruction::Binary {
                op,
                dest: UrclDestination::Branch(dest),
                source1,
                source2,
            } => UrclInstruction::Binary {
                op,
                dest: UrclDestination::Branch(lower(dest)),
                source1,
                source2,
            },
            other => other,
        }
    }
    instructions
}

fn std_instructions<'a>(
    funcs: &mut BTreeMap<&'a str, Function<'a>>,
    sigs: &mut HashMap<&'a str, (StackBehaviour, bool)>,
) {
    // i spent WAY TOO FUCKING LONG on making this macro
    macro_rules! inst {
        ($name:ident $in:literal -> $out:literal { $($i:tt)* } $(branch { $($b:tt)* })?) => {
            {
                let (is_branch, branch) = inst!(br $($($b)*)?);
                let f = Function {
                    name: stringify!($name),
                    stack: stack!($in; -> $out),
                    body: FunctionBody::Urcl {
                        instructions: inst!(e $($i)*),
                        branch,
                    },
                    pos: Position { row: 0, column: 0 },
                };
                sigs.insert(f.name, (f.stack, is_branch));
                funcs.insert(f.name, f);
            }
        };
        (i $inst:ident :dest $($s:tt)*) => {
            inst!(p { $inst (UrclDestination::Branch(UrclBranchDestination::BranchLabel)) } $($s)*)
        };
        (i $inst:ident R($r:literal) $($s:tt)*) => {
            inst!(p { $inst (UrclDestination::Register($r)) } $($s)*)
        };
        (s ($($t:tt)*) R($s:literal) $($r:tt)*) => {
            inst!(p { $($t)* (UrclSource::Register($s)) } $($r)*)
        };
        (s ($($t:tt)*) $s:literal $($r:tt)*) => {
            inst!(p { $($t)* (UrclSource::Literal(Literal::Num($s))) } $($r)*)
        };
        (s ($($t:tt)*) @$s:ident $($r:tt)*) => {
            inst!(p { $($t)* (UrclSource::Literal(Literal::Macro(stringify!($s)))) } $($r)*)
        };
        (d $inst:ident ($dest:expr) ($s1:expr) ($s2:expr)) => {
            UrclInstruction::Binary {
                op: stringify!($inst),
                dest: $dest,
                source1: $s1,
                source2: $s2,
            }
        };
        (d $inst:ident ($dest:expr) ($s:expr)) => {
            UrclInstruction::Unary {
                op: stringify!($inst),
                dest: $dest,
                source: $s,
            }
        };
        (p { $inst:ident $(($s:expr))+ } $($t:tt)+) => {
            inst!(s ($inst $(($s))+) $($t)+)
        };
        (p { $inst:ident $(($s:expr))+ }) => {
            inst!(d $inst $(($s))+)
        };
        (e) => { vec![] };
        (e $($i:tt)+) => {
            vec![
                UrclInstructionEntry {
                    instruction: inst!(i $($i)*),
                    pos: Position { row: 0, column: 0 },
                }
            ]
        };
        (br) => { (false, None) };
        (br $($i:tt)+ ) => { (true, Some(inst!(e $($i)+))) };
    }

    inst!(dup 1 -> 2 { MOV R(2) R(1) });
    // pop is a compile-time operation that only modifies stack height, as anything in unused regs are just garbage
    inst! { pop 1 -> 0 { } }
    // nop doesn't need to map to NOP, since URCL is apparently supposed to support multiple labels per instruction [citation needed]
    inst! { nop 0 -> 0 { } }

    inst! { load 1 -> 1 { LOD R(1) R(1) } }
    inst! { store 2 -> 0 { STR R(1) R(2) } }
    inst! { copy 2 -> 0 { CPY R(1) R(2) } }

    inst! { bool 1 -> 1 { SETNZ R(1) R(1) } branch { BNZ :dest R(1) R(2) } }
    inst! { not 1 -> 1 { NOT R(1) R(1) } branch { BRE :dest R(1) 0 } }

    inst! { xor 2 -> 1 { XOR R(1) R(1) R(2) } }
    inst! { xnor 2 -> 1 { XNOR R(1) R(1) R(2) } }

    inst! { and 2 -> 1 { AND R(1) R(1) R(2) } }
    inst! { nand 2 -> 1 { NAND R(1) R(1) R(2) } }

    inst! { or 2 -> 1 { OR R(1) R(1) R(2) } }
    inst! { nor 2 -> 1 { NOR R(1) R(1) R(2) } }

    inst! { add 2 -> 1 { ADD R(1) R(1) R(2) } }
    inst! { sub 2 -> 1 { SUB R(1) R(1) R(2) } }

    inst! { inc 1 -> 1 { INC R(1) R(1) } }
    inst! { dec 1 -> 1 { DEC R(1) R(1) } }

    inst! { carry 2 -> 1 { SETC R(1) R(1) R(2) } branch { BRC :dest R(1) R(2) } }
    inst! { neg 1 -> 1 { NEG R(1) R(1) } }

    inst! { mult 2 -> 1 { MLT R(1) R(1) R(2) } }
    inst! { sdiv 2 -> 1 { SDIV R(1) R(1) R(2) } }
    inst! { div 2 -> 1 { DIV R(1) R(1) R(2) } }
    inst! { mod 2 -> 1 { MOD R(1) R(1) R(2) } }

    inst! { rsh 1 -> 1 { RSH R(1) R(1) } }
    inst! { ash 1 -> 1 { SRS R(1) R(1) } }
    inst! { lsh 1 -> 1 { LSH R(1) R(1) } }

    inst! { brsh 2 -> 1 { BSR R(1) R(1) R(2) } }
    inst! { bash 2 -> 1 { BSS R(1) R(1) R(2) } }
    inst! { blsh 2 -> 1 { BSL R(1) R(1) R(2) } }

    inst! { eq 2 -> 1 { SETE R(1) R(1) R(2) } branch { BRE :dest R(1) R(2) } }
    inst! { ne 2 -> 1 { SETNE R(1) R(1) R(2) } branch { BNE :dest R(1) R(2) } }

    inst! { sgt 2 -> 1 { SSETG R(1) R(1) R(2) } branch { SBRG :dest R(1) R(2) } }
    inst! { sgte 2 -> 1 { SSETGE R(1) R(1) R(2) } branch { SBGE :dest R(1) R(2) } }
    inst! { slt 2 -> 1 { SSETL R(1) R(1) R(2) } branch { SBRL :dest R(1) R(2) } }
    inst! { slte 2 -> 1 { SSETLE R(1) R(1) R(2) } branch { SBLE :dest R(1) R(2) } }

    inst! { gt 2 -> 1 { SETG R(1) R(1) R(2) } branch { BRG :dest R(1) R(2) } }
    inst! { gte 2 -> 1 { SETGE R(1) R(1) R(2) } branch { BGE :dest R(1) R(2) } }
    inst! { lt 2 -> 1 { SETL R(1) R(1) R(2) } branch { BRL :dest R(1) R(2) } }
    inst! { lte 2 -> 1 { SETLE R(1) R(1) R(2) } branch { BLE :dest R(1) R(2) } }
}
