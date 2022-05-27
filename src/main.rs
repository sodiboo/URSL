mod common;
mod mangle;
mod permutation;
mod urcl;
mod ursl;

pub use common::*;
pub use permutation::*;

use clap::Parser;
use num::Num;
use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
    fs::{self, File},
    io::{self, Write},
};
use tree_sitter::{Node, TreeCursor};

pub trait NodeExt {
    fn pos(&self) -> Position;
    fn end_pos(&self) -> Position;
    fn text<'a>(&self, source: &'a str) -> &'a str;
    fn field(&self, name: &str) -> Self;
}

impl NodeExt for Node<'_> {
    fn pos(&self) -> Position {
        self.start_position().into()
    }

    fn end_pos(&self) -> Position {
        self.end_position().into()
    }

    fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.byte_range()]
    }

    fn field(&self, name: &str) -> Self {
        self.child_by_field_name(name)
            // breaks "expect" convention, but this is also expected to be None very often, so the panic message should be user-facing
            .expect("Badly formatted syntax tree")
    }
}

pub trait StrExt {
    fn trim1(&self, ch: char) -> &Self;
}

impl StrExt for str {
    // This function is used to trim off the leading, irrelevant character in terminal symbols
    // The reason they're not just fields is so labels don't match `:   name`, but only `:name`
    // likewise @macro .label $func #0 $0 shouldn't match @ macro . label $ func # 0 $ 0
    // and this function is used to trim that leading char
    fn trim1(&self, ch: char) -> &Self {
        assert_eq!(self.chars().nth(0).unwrap(), ch);
        &self[1..]
    }
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
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

    /// Allocates locals in bulk by subtracting the desired amount from the stack pointer. By default, they are overwritten to be zero when used. Bulk allocation will be somewhat faster, especially with many locals, but changes code behaviour. As such, this is an optimization that is only safe when your code definitely assigns locals before reading them. Some URCL environments may check for stack overflow with PSH, and they may not catch stack overflows with this option set either. As such, this is an optimization that should only be done when you're sure your code is non-recursive and you want to prioritize stack code size at all costs
    #[clap(long)]
    garbage_initialized_locals: bool,

    /// Removes predefined instructions. With this parameter, only the following instructions are predefined: const, in, out, jump, branch, halt, call, ret, get, set
    #[clap(short, long)]
    minimal: bool,
}

pub struct Headers {
    bits: u64,
    minheap: usize,
    minstack: usize,
}

fn main() -> io::Result<()> {
    let mut args = Args::parse();
    if args.emit_chars_as_numbers {
        args.emit_chars_literally = true;
    }
    let source = &fs::read_to_string(&args.input).unwrap();
    let tree = {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(tree_sitter_ursl::language())
            .expect("Error loading language data");
        parser
            .parse(source, None)
            .expect("Badly formatted syntax tree")
    };
    let tree = tree.root_node();
    let headers = &parse_headers(tree.field("headers"), source);
    let cursor = &mut tree.walk();
    let args = &args;
    let defs = {
        let mut defs = Vec::<(&str, DefValue)>::new();
        for node in tree.children_by_field_name("data", cursor).collect::<Vec<_>>() {
            let label = node.field("label").text(source).trim1('.');
            let value_node = node.field("value");
            let value: DefValue = match value_node.kind() {
                "array" => DefValue::Array(
                    value_node
                        .children_by_field_name("items", cursor)
                        .map(|node| lower_literal(&args, headers, parse_literal(node, &source)))
                        .collect(),
                ),
                _ => DefValue::Single(lower_literal(
                    args,
                    headers,
                    parse_literal(value_node, &source),
                )),
            };
            defs.push((label, value));
        }
        defs
    };

    if args.verbose {
        for (label, val) in &defs {
            println!(".{label} {val}");
        }
    }

    let functions = parse_functions(
        args,
        headers,
        tree.children_by_field_name("code", cursor).collect(),
        source,
        cursor,
    );

    let main_locals = if let Some(main) = functions.get("$main") {
        assert_eq!(main.stack.input, 0, "$main may not take any arguments");
        assert_eq!(main.stack.output, 0, "$main may not return any values");
        if let FunctionBody::Ursl { locals, .. } = main.body {
            locals
        } else {
            panic!("$main is a custom instruction. This should be unreachable.")
        }
    } else {
        panic!("No $main function")
    };

    let mut f = File::create(&args.output)?;

    writeln!(f, "BITS {}", headers.bits)?;
    writeln!(f, "MINHEAP {}", headers.minheap)?;
    writeln!(f, "MINSTACK {}", headers.minstack)?;

    writeln!(f, "PSH ~+2")?;
    if main_locals != 0 {
        writeln!(f, "SUB SP SP {main_locals}")?;
    }
    writeln!(f, "JMP .{}", mangle::function_name("main"))?;
    writeln!(f, "HLT")?;

    for (label, val) in defs {
        writeln!(f, ".{} {val}", mangle::data_label(label))?;
    }

    for func in functions.values() {
        if let FunctionBody::Ursl {
            locals,
            ref instructions,
        } = func.body
        {
            ursl::emit_instructions(args, &mut f, &functions, func, locals, instructions)?
        }
    }

    Ok(())
}

fn parse_headers(node: Node, source: &str) -> Headers {
    assert_eq!(node.kind(), "headers");
    Headers {
        bits: parse_num(node.field("bits").text(source)),
        minheap: parse_num(node.field("minheap").text(source)),
        minstack: parse_num(node.field("minstack").text(source)),
    }
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

fn parse_locals(node: Node, source: &str) -> usize {
    match node.child_by_field_name("locals") {
        Some(node) => parse_num(node.text(source)),
        None => 0,
    }
}

fn parse_label<'a, T: PositionEntry>(
    inst: Node,
    labels: &HashMap<&'a str, usize>,
    instructions: &Vec<T>,
    func_name: &'a str,
    source: &'a str,
) -> Option<&'a str> {
    match inst.child_by_field_name("label") {
        Some(node) => {
            let label = Some(&node.text(source)[1..]); // trim :
            if let Some(label) = &label {
                if let Some(idx) = labels.get(label) {
                    panic!(
                        "Duplicate label {}:{} at {} and {}",
                        func_name,
                        label,
                        instructions[*idx].pos(),
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
    headers: &Headers,
    funcs: Vec<Node<'a>>,
    source: &'a str,
    cursor: &mut TreeCursor<'a>,
) -> BTreeMap<&'a str, Function<'a>> {
    // btreemap ensures deterministic ordering when writing output
    let mut functions = BTreeMap::<&'a str, Function<'a>>::new();
    let mut signatures = HashMap::<&'a str, (StackBehaviour, bool)>::new();
    let mut instruction_nodes = HashMap::new();

    if !args.minimal {
        std_instructions(&mut functions, &mut signatures);
    }

    for node in funcs {
        match node.kind() {
            "func" => {
                let stack = parse_stack_sig(node, source);
                let locals = parse_locals(node, source);
                let name = node.field("name").text(source); // don't trim $, that way it doesn't collide with insts
                functions.insert(
                    name,
                    Function {
                        name,
                        stack,
                        body: FunctionBody::Ursl {
                            locals,
                            instructions: Vec::new(),
                        },
                        pos: node.pos(),
                    },
                );
                signatures.insert(name, (stack, false));
                instruction_nodes.insert(name, node);
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
                let instructions = urcl::parse_instructions(
                    args,
                    headers,
                    node.children_by_field_name("instructions", cursor).collect(),
                    name,
                    None,
                    source,
                    cursor,
                );
                let branch = node.child_by_field_name("branch").map(|branch_clause| {
                    urcl::parse_instructions(
                        args,
                        headers,
                        branch_clause.children_by_field_name("instructions", cursor).collect(),
                        name,
                        Some(&branch_clause.field("label").text(source)[1..]), // trim :
                        source,
                        cursor,
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
                        name,
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
            "inst_permutation" => {
                let name = node.field("name").text(source);
                let perm = parse_permutation_sig(node.field("permutation"), source, cursor);
                let instructions = compile_permutation(&perm, node.pos());
                let stack = stack!(perm.input; -> perm.output.len());
                functions.insert(
                    name,
                    Function {
                        name,
                        stack,
                        body: FunctionBody::Urcl {
                            instructions,
                            branch: None,
                        },
                        pos: node.pos(),
                    },
                );
                signatures.insert(name, (stack, false));
            }
            _ => panic!(
                "Unknown node kind, expected func or inline. Error at {}",
                node.pos()
            ),
        }
    }

    for func in functions.values_mut() {
        match &mut func.body {
            FunctionBody::Ursl {
                locals,
                instructions,
            } => {
                let locals = *locals;
                let node = instruction_nodes.remove(func.name).unwrap();
                ursl::parse_instructions(
                    args,
                    headers,
                    &signatures,
                    node.children_by_field_name("instructions", cursor).collect(),
                    func.name,
                    func.stack.input + locals,
                    func.stack.output,
                    instructions,
                    source,
                    cursor,
                );
                if args.verbose {
                    println!("func {} : {} + {locals} {{", func.name, func.stack);
                    for entry in instructions {
                        if let Some(label) = &entry.label {
                            println!(" :{label}")
                        }
                        println!("  {}", entry.instruction);
                        match entry.instruction {
                            ursl::Instruction::Ret
                            | ursl::Instruction::Halt
                            | ursl::Instruction::Jump(_)
                            | ursl::Instruction::Branch(_, _) => println!(),
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

fn std_instructions<'a>(
    funcs: &mut BTreeMap<&'a str, Function<'a>>,
    sigs: &mut HashMap<&'a str, (StackBehaviour, bool)>,
) {
    // i spent WAY TOO FUCKING LONG on making this macro
    macro_rules! inst {
        ($name:ident $in:literal -> $out:literal { $($i:tt)* } $(branch { $($b:tt)* })?) => {{
            let (is_branch, branch) = inst!(br $($($b)*)?);
            let f = Function {
                name: stringify!($name),
                stack: stack!($in; -> $out),
                body: FunctionBody::Urcl {
                    instructions: inst!(e $($i)*),
                    branch,
                },
                pos: Default::default(),
            };
            sigs.insert(f.name, (f.stack, is_branch));
            funcs.insert(f.name, f);
        }};
        ($name:ident [$($in:ident)*] -> [$($out:ident)*]) => {{
            let mut names = HashMap::new();
            for (i, name) in <[&str]>::iter(&[$(stringify!($in),)*]).enumerate() {
                names.insert(name, i);
            }
            let p = Permutation {
                input: names.len(),
                output: vec![$(names[&stringify!($out)],)*],
            };
            let f = Function {
                name: stringify!($name),
                stack: stack!(p.input; -> p.output.len()),
                body: FunctionBody::Urcl {
                    instructions: compile_permutation(&p, Default::default()),
                    branch: None,
                },
                pos: Default::default(),
            };
            sigs.insert(f.name, (f.stack, false));
            funcs.insert(f.name, f);
        }};
        (i $inst:ident :dest $($s:tt)*) => {
            inst!(p { $inst (urcl::Destination::Branch(urcl::BranchDestination::BranchLabel)) } $($s)*)
        };
        (i $inst:ident R($r:literal) $($s:tt)*) => {
            inst!(p { $inst (urcl::Destination::Register($r)) } $($s)*)
        };
        (s ($($t:tt)*) R($s:literal) $($r:tt)*) => {
            inst!(p { $($t)* (urcl::Source::Register($s)) } $($r)*)
        };
        (s ($($t:tt)*) $s:literal $($r:tt)*) => {
            inst!(p { $($t)* (urcl::Source::Literal(Literal::Num($s))) } $($r)*)
        };
        (s ($($t:tt)*) @$s:ident $($r:tt)*) => {
            inst!(p { $($t)* (urcl::Source::Literal(Literal::Macro(stringify!($s)))) } $($r)*)
        };
        (d $inst:ident ($dest:expr) ($s1:expr) ($s2:expr)) => {
            urcl::Instruction::Binary {
                op: stringify!($inst),
                dest: $dest,
                source1: $s1,
                source2: $s2,
            }
        };
        (d $inst:ident ($dest:expr) ($s:expr)) => {
            urcl::Instruction::Unary {
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
                urcl::InstructionEntry {
                    instruction: inst!(i $($i)*),
                    pos: Default::default(),
                }
            ]
        };
        (br) => { (false, None) };
        (br $($i:tt)+ ) => { (true, Some(inst!(e $($i)+))) };
    }

    inst!(dup [a] -> [a a]);
    // pop is a compile-time operation that only modifies stack height, as anything in unused regs are just garbage
    inst! { pop [a] -> [] }
    // nop doesn't need to map to NOP, since URCL is apparently supposed to support multiple labels per instruction [citation needed]
    inst! { nop [] -> [] }

    inst! { load 1 -> 1 { LOD R(1) R(1) } }
    inst! { store 2 -> 0 { STR R(1) R(2) } }
    inst! { copy 2 -> 0 { CPY R(1) R(2) } }

    inst! { bool 1 -> 1 { SETNZ R(1) R(1) } branch { BNZ :dest R(1) } }
    inst! { not 1 -> 1 { NOT R(1) R(1) } branch { BNE :dest R(1) @MAX } }

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
