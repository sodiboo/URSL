use std::fmt::{Display, Formatter, Result};

use tree_sitter::Point;

#[macro_export]
macro_rules! stack {
    ($in:expr; -> $out:expr) => {
        StackBehaviour {
            input: $in,
            output: $out,
        }
    };
}

#[derive(Copy, Clone)]
pub struct StackBehaviour {
    pub input: u64,
    pub output: u64,
}

impl Display for StackBehaviour {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} -> {}", self.input, self.output)
    }
}

pub struct Position {
    pub row: usize,
    pub column: usize,
}

impl From<Point> for Position {
    fn from(p: Point) -> Self {
        Self {
            row: p.row + 1,
            column: p.column + 1,
        }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "[{}:{}]", self.row, self.column)
    }
}

pub struct Function<'a> {
    pub name: &'a str,
    pub stack: StackBehaviour,
    pub body: FunctionBody<'a>,
    pub pos: Position,
}

pub enum FunctionBody<'a> {
    Ursl {
        locals: u64,
        instructions: Vec<InstructionEntry<'a>>,
    },
    Urcl {
        instructions: Vec<UrclInstructionEntry<'a>>,
        branch: Option<Vec<UrclInstructionEntry<'a>>>,
    },
}

pub struct InstructionEntry<'a> {
    pub label: Option<&'a str>,
    pub excess_height: u64,
    pub enter_height: u64,
    pub exit_height: Option<u64>,
    pub instruction: Instruction<'a>,
    pub pos: Position,
}

pub struct UrclInstructionEntry<'a> {
    pub instruction: UrclInstruction<'a>,
    pub pos: Position,
}

#[derive(Clone)]
pub enum UrclInstruction<'a> {
    In {
        dest: u64,
        port: &'a str,
    },
    Out {
        port: &'a str,
        source: UrclSource<'a>,
    },
    Jmp {
        dest: UrclBranchDestination<'a>,
    },
    Unary {
        op: &'a str,
        dest: UrclDestination<'a>,
        source: UrclSource<'a>,
    },
    Binary {
        op: &'a str,
        dest: UrclDestination<'a>,
        source1: UrclSource<'a>,
        source2: UrclSource<'a>,
    },
}

impl Display for UrclInstruction<'_> {
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
pub enum UrclDestination<'a> {
    Register(u64),
    Branch(UrclBranchDestination<'a>),
}

impl UrclDestination<'_> {
    pub fn emit(&self, excess_height: u64, branch_target: Option<&str>) -> String {
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

impl Display for UrclDestination<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.emit(0, None))
    }
}

#[derive(Clone, Copy)]
pub enum UrclBranchDestination<'a> {
    TemporaryLabel(&'a str),
    Relative(i64),
    BranchLabel,
}

impl UrclBranchDestination<'_> {
    pub fn emit(&self, branch_target: Option<&str>) -> String {
        match self {
            Self::BranchLabel => match branch_target {
                Some(t) => format!(".{t}"),
                None => format!("{{BRANCH TARGET}}"),
            },
            Self::TemporaryLabel(label) => format!("{{TEMPORARY LABEL :{}}}", *label),
            Self::Relative(n) if *n >= 0 => format!("~+{n}"),
            Self::Relative(n) => format!("~{n}"),
        }
    }
}

impl Display for UrclBranchDestination<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.emit(None))
    }
}

#[derive(Clone)]
pub enum UrclSource<'a> {
    Register(u64),
    Literal(Literal<'a>),
}

impl UrclSource<'_> {
    pub fn emit(&self, excess_height: u64) -> String {
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

impl Display for UrclSource<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.emit(0))
    }
}

pub enum DefValue<'a> {
    Single(Literal<'a>),
    Array(Vec<Literal<'a>>),
}

impl Display for DefValue<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Single(element) => element.fmt(f),
            Self::Array(elements) => {
                write!(f, "[ ")?;
                for element in elements {
                    write!(f, "{} ", element)?;
                }
                write!(f, "]")
            }
        }
    }
}

#[derive(Clone)]
pub enum Literal<'a> {
    Char(char),
    CharEscape(char),
    Macro(&'a str),
    Num(u64),
    Mem(u64),
    Label(&'a str),
    Func(&'a str),
}

impl Display for Literal<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Char(ch) => write!(f, "'{ch}'"),
            Self::CharEscape(ch) => write!(f, "'\\{ch}'"),
            Self::Macro(name) => write!(f, "@{name}"),
            Self::Num(n) => write!(f, "{n}"),
            Self::Mem(addr) => write!(f, "#{addr}"),
            Self::Label(name) => write!(f, ".{}", super::mangle_data_label(name)),
            Self::Func(name) => write!(f, ".{}", super::mangle_function_name(name)),
        }
    }
}

pub enum Instruction<'a> {
    Const(Literal<'a>),

    In(&'a str),
    Out(&'a str),

    Jump(&'a str),
    Branch(&'a str, &'a str),

    Halt,

    Call(&'a str),
    IndirectCall(StackBehaviour),
    Ret,

    Stack(u64),

    Let(Vec<&'a str>, Vec<InstructionEntry<'a>>),
    Get(&'a str),
    Set(&'a str),
}

impl Display for Instruction<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Const(lit) => write!(f, "const {lit}"),

            Self::In(port) => write!(f, "in %{port}"),
            Self::Out(port) => write!(f, "out %{port}"),

            Self::Jump(dest) => write!(f, "jump :{dest}"),
            Self::Branch(condition, dest) => write!(f, "{condition} branch :{dest}"),

            Self::Halt => write!(f, "halt"),

            Self::Call(func) => write!(f, "call {func}"),
            Self::IndirectCall(stack) => write!(f, "icall {stack}"),
            Self::Ret => write!(f, "ret"),

            Self::Stack(idx) => write!(f, "stack {idx}"),
            Self::Let(names, insts) => {
                write!(f, "let")?;
                for name in names {
                    write!(f, " {name}")?;
                }
                writeln!(f, " {{")?;
                for entry in insts {
                    writeln!(f, "{}", entry.instruction)?;
                }
                write!(f, "}}")
            }
            Self::Get(op) => write!(f, "get ${op}"),
            Self::Set(op) => write!(f, "set ${op}"),
        }
    }
}
