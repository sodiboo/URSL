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
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    fmt::Debug,
    fs::{self, File},
    io::{self, Write},
    iter,
};
use tree_sitter::{Node, Tree};

pub trait NodeExt<'a> {
    fn pos(&self, unit: &'a CompilationUnit<'a>) -> Position<'a>;
    fn text(&self, unit: &'a CompilationUnit<'a>) -> &'a str;
    fn field(&self, name: &str, unit: &'a CompilationUnit<'a>) -> Self;
}

impl<'a> NodeExt<'a> for Node<'a> {
    fn pos(&self, unit: &'a CompilationUnit<'a>) -> Position<'a> {
        Position {
            unit,
            range: self.range(),
        }
    }

    fn text(&self, unit: &'a CompilationUnit<'a>) -> &'a str {
        &unit.source[self.byte_range()]
    }

    fn field(&self, name: &str, unit: &'a CompilationUnit<'a>) -> Self {
        self.child_by_field_name(name)
            // breaks "expect" convention, but this is also expected to be None very often, so the panic message should be user-facing
            .unwrap_or_else(|| {
                panic!(
                    "Badly formatted syntax tree; expected a field `{name}` as child of `{}` at {}",
                    self.kind(),
                    self.pos(unit)
                )
            })
    }
}

#[derive(Parser, Debug)]
#[clap(author, version, about)]
pub struct CliArgs {
    #[clap(short, long = "input-file")]
    input: String,

    #[clap(short, long = "output-file")]
    output: String,

    #[clap(flatten)]
    args: Args,

    /// Fuck it. Try emitting URCL despite any errors that may have occurred.
    #[clap(long)]
    fuck_it: bool,
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
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

    /// Do not import prelude. Only the intrinsic instructions are predefined: const, in, out, jump, branch, halt, call, ret, get, set
    #[clap(long)]
    no_prelude: bool,

    /// Do not enforce $main to exist or have a particular signature. Do not call $main at the start
    #[clap(long)]
    no_main: bool,
}

pub struct Headers {
    bits: u64,
    minheap: usize,
    minstack: usize,
}

// fuck this lint
#[allow(non_upper_case_globals)]
const prelude_source: &str = include_str!("prelude.ursl");

fn main() -> io::Result<()> {
    let mut cli = CliArgs::parse();
    if cli.args.emit_chars_as_numbers {
        cli.args.emit_chars_literally = true;
    }
    let source = &fs::read_to_string(&cli.input)?;

    let parser = &mut tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_ursl::language())
        .expect("Failed to set language. For sure unreachable.");
    let prelude = CompilationUnit::new("<prelude>", prelude_source, parser);
    let main = CompilationUnit::new(&cli.input, source, parser);

    let headers = parse_headers(
        main.tree
            .root_node()
            .children_by_field_name("headers", &mut main.tree.walk()),
        &main,
    );

    let (result, errors) = compile(&cli.args, headers, &[&prelude, &main]);

    if !errors.is_empty() {
        let max_line_no_width = source.lines().count().to_string().len();
        let err_count = errors.len();
        eprintln!();
        for SourceError { pos, message } in errors {
            if let Some(pos) = pos {
                eprintln!("{:>>max_line_no_width$} {pos}", "");
                if pos.range.start_point.row == pos.range.end_point.row {
                    let row = pos.range.start_point.row + 1;
                    let line = source
                        .lines()
                        .nth(pos.range.start_point.row)
                        .expect("Error printing errors");
                    let start = pos.range.start_point.column;
                    let end = pos.range.end_point.column;
                    let err_pointer: String = iter::repeat(' ')
                        .take(start)
                        .chain(iter::repeat('^'))
                        .take(end)
                        .collect();
                    eprintln!("{row:>max_line_no_width$} | {line}");
                    eprintln!("{spc:>max_line_no_width$}   {err_pointer}", spc = ' ');
                } else {
                    let lines = source
                        .lines()
                        .enumerate()
                        .skip(pos.range.start_point.row)
                        .take(pos.range.end_point.row - pos.range.start_point.row);
                    for (row, line) in lines {
                        let row = row + 1;
                        eprintln!("{row:>max_line_no_width$} | {line}");
                    }
                }
                eprintln!("{:<<max_line_no_width$} {message}", "");
            } else {
                eprintln!("{message}");
            }
            eprintln!();
        }
        eprintln!("{err_count} errors");
        if cli.fuck_it {
            eprintln!("The partial data that the compiler has will now be emitted as if nothing went wrong.");
            eprintln!("This will likely panic.");
            eprintln!("If it does not panic, the output will likely be garbage.");
            eprintln!("You asked for this. Blame yourself.");
            eprintln!();
        } else {
            eprintln!("Compilation failed.");
            eprintln!();
            std::process::exit(1);
        }
    }

    let mut output_file = File::create(&cli.output)?;
    emit(&mut output_file, &cli.args, result)
}

struct CompileResult<'a> {
    headers: Headers,
    defs: Vec<(&'a str, DefValue<'a>)>,
    functions: BTreeMap<&'a str, Function<'a>>,
}

pub struct CompilationUnit<'a> {
    path: &'a str,
    source: &'a str,
    tree: Tree,
}

impl<'a> CompilationUnit<'a> {
    pub fn new(path: &'a str, source: &'a str, parser: &mut tree_sitter::Parser) -> Self {
        let tree = parser
            .parse(source, None)
            .unwrap_or_else(|| panic!("Parsing fucked up real bad in {path}. Didn't even give me a syntax tree. This should be impossible."));
        CompilationUnit { path, source, tree }
    }
}

fn compile<'a>(
    args: &Args,
    headers: Headers,
    units: &[&'a CompilationUnit<'a>],
) -> (CompileResult<'a>, Vec<SourceError<'a>>) {
    let mut errors = Vec::new();
    let mut defs = Vec::<(&str, DefValue)>::new();
    let mut functions = BTreeMap::new();
    let mut signatures = HashMap::new();
    for unit in units {
        for node in unit
            .tree
            .root_node()
            .children_by_field_name("data", &mut unit.tree.walk())
        {
            let label = node.field("label", unit).field("name", unit).text(unit);
            let value_node = node.field("value", unit);
            let value: DefValue = match value_node.kind() {
                "array" => DefValue::Array(
                    value_node
                        .children_by_field_name("items", &mut unit.tree.walk())
                        .map(|node| lower_literal(&args, &headers, parse_literal(node, unit)))
                        .collect(),
                ),
                _ => DefValue::Single(lower_literal(
                    args,
                    &headers,
                    parse_literal(value_node, unit),
                )),
            };
            defs.push((label, value));
        }

        if args.verbose {
            for (label, val) in &defs {
                println!(".{label} {val}");
            }
        }

        errors.extend(parse_functions(
            args,
            &headers,
            unit.tree
                .root_node()
                .children_by_field_name("code", &mut unit.tree.walk()),
            &mut functions,
            &mut signatures,
            unit,
        ));
    }

    if args.no_main {
        // ignore these checks lol
    } else if let Some(main) = functions.get("$main") {
        if main.stack.input != 0 {
            err!(errors; main.unit; main.node.field("stack", main.unit).field("params", main.unit), "$main may not take any arguments");
        }
        if main.stack.output != 0 {
            err!(errors; main.unit; main.node.field("stack", main.unit).field("returns", main.unit), "$main may not return any values");
        }
    } else {
        err!(errors; None, "No $main function")
    };
    errors.sort_by(|a, b| {
        if let Some(ref a) = a.pos {
            if let Some(ref b) = b.pos {
                a.range.start_point.row.cmp(&b.range.start_point.row)
            } else {
                Ordering::Greater
            }
        } else if let Some(_) = b.pos {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    });
    (
        CompileResult {
            headers,
            defs,
            functions,
        },
        errors,
    )
}

fn emit(f: &mut impl Write, args: &Args, result: CompileResult) -> io::Result<()> {
    writeln!(f, "BITS {}", result.headers.bits)?;
    writeln!(f, "MINHEAP {}", result.headers.minheap)?;
    writeln!(f, "MINSTACK {}", result.headers.minstack)?;

    let mut max_regs = 0;

    let mut contents = Vec::new();
    if !args.no_main {
        writeln!(contents, "CAL .{}", mangle::function_name("main"))?;
        writeln!(contents, "HLT")?;
    }

    for (label, val) in result.defs {
        writeln!(contents, ".{}\nDW {val}", mangle::data_label(label))?;
    }

    for func in result.functions.values() {
        if let FunctionBody::Ursl {
            locals,
            ref instructions,
        } = func.body
        {
            ursl::emit_instructions(
                args,
                &mut contents,
                &result.functions,
                func,
                locals,
                instructions,
                &mut max_regs,
            )?
        }
    }

    writeln!(f, "MINREG {max_regs}")?;
    f.write_all(&contents)
}

fn parse_headers<'a>(
    headers: impl Iterator<Item = Node<'a>>,
    unit: &'a CompilationUnit<'a>,
) -> Headers {
    macro_rules! parse_headers {
        ($($name:ident)*) => {{
            $(let mut $name = None;)*
            for header in headers {
                match header.kind() {
                    $(stringify!($name) =>
                        if $name.replace(
                                header
                                    .field("value", unit)
                                    .text(unit)
                                    .parse()
                                    .expect(concat!("Invalid value for header `", stringify!($name), "`"))
                            ).is_some()
                        {
                            panic!(concat!("Duplicate header `", stringify!($name), "`"))
                        }
                    )*
                    _ => unknown_node(header, unit),
                }
            }
            $(let $name = $name.expect(concat!("Missing header `", stringify!($name), "`"));)*
            Headers { $($name,)* }
        }};
    }
    parse_headers!(bits minheap minstack)
}

fn parse_stack_sig(node: Node, unit: &CompilationUnit) -> StackBehaviour {
    match node.child_by_field_name("stack") {
        Some(node) => parse_stack(node, unit),
        None => stack!(0; -> 0),
    }
}

fn parse_stack(node: Node, unit: &CompilationUnit) -> StackBehaviour {
    StackBehaviour {
        input: parse_num(node.field("params", unit).text(unit)),
        output: parse_num(node.field("returns", unit).text(unit)),
    }
}

fn parse_locals(node: Node, unit: &CompilationUnit) -> usize {
    match node.child_by_field_name("locals") {
        Some(node) => parse_num(node.text(unit)),
        None => 0,
    }
}

fn parse_functions<'a>(
    args: &Args,
    headers: &Headers,
    funcs: impl Iterator<Item = Node<'a>>,
    functions: &mut BTreeMap<&'a str, Function<'a>>,
    signatures: &mut HashMap<&'a str, (StackBehaviour, bool)>,
    unit: &'a CompilationUnit<'a>,
) -> Vec<SourceError<'a>> {
    let mut errors = Vec::new();
    // btreemap ensures deterministic ordering when writing output
    let mut instruction_nodes = HashMap::new();

    for node in funcs {
        match node.kind() {
            "func" => {
                let stack = parse_stack_sig(node, unit);
                let locals = parse_locals(node, unit);
                let name = node.field("name", unit).text(unit); // don't trim $, that way it doesn't collide with insts
                functions.insert(
                    name,
                    Function {
                        node,
                        name,
                        stack,
                        body: FunctionBody::Ursl {
                            locals,
                            instructions: Vec::new(),
                        },
                        pos: node.pos(unit),
                        unit,
                    },
                );
                signatures.insert(name, (stack, false));
                instruction_nodes.insert(name, node);
            }
            "inst" => {
                let name = node.field("name", unit).text(unit);
                if ["halt", "ret"].contains(&name) {
                    err!(errors; unit; node.field("name", unit), "inst {name} is also defined as intrinsic");
                }
                let input = urcl::parse_stack_bindings(
                    node.children_by_field_name("input", &mut unit.tree.walk()),
                    unit,
                );
                let output = urcl::parse_stack_bindings(
                    node.children_by_field_name("output", &mut unit.tree.walk()),
                    unit,
                );
                let stack = stack!(input.len(); -> output.len());
                let instructions = urcl::parse_instructions(
                    args,
                    headers,
                    node.field("instructions", unit),
                    name,
                    None,
                    unit,
                )
                .extend_into(&mut errors);
                let body = UrclMainBody {
                    input,
                    output,
                    instructions,
                    pos: node.pos(unit),
                };
                if let Some(Function {
                    node: _,
                    name: _,
                    stack: old_stack,
                    body: f_body,
                    unit: _,
                    pos: old_pos,
                }) = functions.get_mut(name)
                {
                    if let FunctionBody::Urcl {
                        overloads,
                        branch: _,
                    } = f_body
                    {
                        if old_stack.input != stack.input {
                            err!(errors; unit; node,
                                "inst {name} is defined with a different signature than before. Here it has {} input items, but before it had {} input items. Previous definition at {old_pos}",
                                stack.input, old_stack.input,
                            );
                        }
                        if old_stack.output != stack.output {
                            err!(errors; unit; node,
                                "inst {name} is defined with a different signature than before. Here it has {} output items, but before it had {} output items. Previous definition at {old_pos}",
                                stack.output, old_stack.output,
                            );
                        }
                        overloads.push(body);
                    } else {
                        err!(errors; unit; node, "inst {name} is also defined at {old_pos}");
                    }
                } else {
                    functions.insert(
                        name,
                        Function {
                            node,
                            name,
                            stack,
                            body: FunctionBody::Urcl {
                                overloads: vec![body],
                                branch: None,
                            },
                            pos: node.pos(unit),
                            unit,
                        },
                    );
                    signatures.insert(name, (stack, false));
                }
            }
            "inst_branch" => {
                let name = node.field("name", unit).text(unit);
                if ["halt", "ret"].contains(&name) {
                    err!(errors; unit; node.field("name", unit), "inst {name} is also defined as intrinsic");
                }
                let input = urcl::parse_stack_bindings(
                    node.children_by_field_name("input", &mut unit.tree.walk()),
                    unit,
                );
                let branch_destination = &node.field("label", unit).field("name", unit).text(unit);
                let stack = stack!(input.len(); -> 1);
                let instructions = urcl::parse_instructions(
                    args,
                    headers,
                    node.field("instructions", unit),
                    name,
                    Some(branch_destination),
                    unit,
                )
                .extend_into(&mut errors);
                let branch = UrclBranchBody {
                    input,
                    instructions,
                    pos: node.pos(unit),
                };
                if let Some(Function {
                    node: _,
                    name: _,
                    stack: old_stack,
                    body: f_body,
                    unit: _,
                    pos: old_pos,
                }) = functions.get_mut(name)
                {
                    if let FunctionBody::Urcl {
                        overloads: _,
                        branch: branch_body,
                    } = f_body
                    {
                        if old_stack.input != stack.input {
                            err!(errors; unit; node,
                                "branch {name} is defined with a different signature than before. Here it has {} input items, but before it had {} input items. Previous definition at {old_pos}",
                                stack.input, old_stack.input,
                            );
                        }
                        if old_stack.output != stack.output {
                            err!(errors; unit; node,
                                "branch {name} is defined with a different signature than before. Here it has {} output items, but before it had {} output items. Previous definition at {old_pos}",
                                stack.output, old_stack.output,
                            );
                        }
                        if let Some(old_branch) = branch_body.replace(branch) {
                            err!(errors; unit; node,
                                "branch {name} is also defined at {}", old_branch.pos);
                        } else {
                            signatures.get_mut(name).unwrap().1 = true;
                        }
                    } else {
                        err!(errors; unit; node, "inst {name} is also defined at {old_pos}");
                    }
                } else {
                    functions.insert(
                        name,
                        Function {
                            node,
                            name,
                            stack,
                            body: FunctionBody::Urcl {
                                overloads: vec![],
                                branch: Some(branch),
                            },
                            pos: node.pos(unit),
                            unit,
                        },
                    );
                    signatures.insert(name, (stack, true));
                }
            }
            "inst_permutation" => {
                let name = node.field("name", unit).text(unit);
                if ["halt", "ret"].contains(&name) {
                    err!(errors; unit; node.field("name", unit), "inst {name} is also defined as intrinsic");
                }
                if let Some(f) = functions.get(&name) {
                    panic!(
                        "inst {name} at {} is also defined at {}",
                        node.pos(unit),
                        f.pos
                    );
                }
                let perm = parse_permutation_sig(node.field("permutation", unit), unit)
                    .extend_into(&mut errors);
                let stack = stack!(perm.input; -> perm.output.len());
                functions.insert(
                    name,
                    Function {
                        node,
                        name,
                        stack,
                        body: FunctionBody::Permutation(perm),
                        pos: node.pos(unit),
                        unit,
                    },
                );
                signatures.insert(name, (stack, false));
            }
            "dunder_binary" => {
                let name = node.field("name", unit).text(unit);
                let instruction = node.field("instruction", unit);
                functions.insert(
                    name,
                    Function {
                        node,
                        name,
                        stack: stack!(2; -> 1),
                        body: FunctionBody::Urcl {
                            overloads: urcl::__binary__(node, instruction, unit),
                            branch: None,
                        },
                        pos: node.pos(unit),
                        unit,
                    },
                );
                signatures.insert(name, (stack!(2; -> 1), false));
            }
            "dunder_branching" => {
                let name = node.field("name", unit).text(unit);
                let instruction = node.field("instruction", unit);
                let branch = node.field("branch", unit);
                functions.insert(
                    name,
                    Function {
                        node,
                        name,
                        stack: stack!(2; -> 1),
                        body: FunctionBody::Urcl {
                            overloads: urcl::__binary__(node, instruction, unit),
                            branch: Some(urcl::__branching__(node, branch, unit)),
                        },
                        pos: node.pos(unit),
                        unit,
                    },
                );
                signatures.insert(name, (stack!(2; -> 1), true));
            }
            _ => unknown_node(node, unit),
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
                errors.extend(ursl::parse_instructions(
                    args,
                    headers,
                    &signatures,
                    node.children_by_field_name("instructions", &mut unit.tree.walk())
                        .collect(),
                    func.name,
                    func.stack.input + locals,
                    func.stack.output,
                    instructions,
                    unit,
                ));
                if args.verbose {
                    println!("func {} : {} + {locals} {{", func.name, func.stack);
                    for entry in instructions {
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
            FunctionBody::Urcl { overloads, branch } => {
                if args.verbose {
                    for UrclMainBody {
                        input,
                        output,
                        instructions,
                        pos: _,
                    } in overloads
                    {
                        print!("inst {}{input}", func.name);
                        if output.len() != 0 {
                            print!(" ->{output}");
                        }
                        println!(" {{");
                        for entry in instructions {
                            println!("  {}", entry.instruction)
                        }
                        println!("}}");
                    }
                    if let Some(UrclBranchBody {
                        input,
                        instructions,
                        pos: _,
                    }) = branch
                    {
                        println!("branch {}{input} {{", func.name);
                        for entry in instructions {
                            println!("  {}", entry.instruction)
                        }
                        println!("}}")
                    }
                }
            }
            FunctionBody::Permutation(perm) => {
                if args.verbose {
                    println!("inst {} {perm}", func.name);
                }
            }
        }
    }
    errors
}
