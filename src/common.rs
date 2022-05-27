use super::*;
use num::BigUint;
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
    pub input: usize,
    pub output: usize,
}

impl Display for StackBehaviour {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} -> {}", self.input, self.output)
    }
}

#[derive(Clone, Copy, Default)]
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
        locals: usize,
        instructions: Vec<ursl::InstructionEntry<'a>>,
    },
    Urcl {
        instructions: Vec<urcl::InstructionEntry<'a>>,
        branch: Option<Vec<urcl::InstructionEntry<'a>>>,
    },
}

pub trait PositionEntry {
    fn pos(&self) -> &Position;
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
    Num(BigUint),
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
            Self::Label(name) => write!(f, ".{}", mangle::data_label(name)),
            Self::Func(name) => write!(f, ".{}", mangle::function_name(name)),
        }
    }
}

pub fn parse_literal<'a>(node: Node, source: &'a str) -> Literal<'a> {
    match node.kind() {
        "char_literal" => {
            let char_literal_value = node.field("value");
            match char_literal_value.kind() {
                "char" => {
                    let char_text = char_literal_value.text(source);
                    if let &[ch] = char_text.chars().collect::<Vec<_>>().as_slice() {
                        Literal::Char(ch)
                    } else {
                        unreachable!("Char literal should only contain 1 char {}", node.pos());
                    }
                }
                "char_escape" => {
                    let char_text = char_literal_value.text(source);
                    if let &[escape, ch] = char_text.chars().collect::<Vec<_>>().as_slice() {
                        assert_eq!(escape, '\\');
                        Literal::CharEscape(ch)
                    } else {
                        unreachable!("Escape sequence should always be 2 chars at {}", node.pos());
                    }
                }
                kind => {
                    unreachable!("Unknown node kind `{kind}` at {}", node.pos());
                }
            }
        }
        "number" => Literal::Num(parse_num(node.text(source))),
        "macro" => Literal::Macro(node.text(source).trim1('@')),
        "data_label" => Literal::Label(node.text(source).trim1('.')),
        "function_name" => Literal::Func(node.text(source).trim1('$')),
        "mem" => Literal::Mem(node.text(source).trim1('#').parse().unwrap()),
        kind => {
            unreachable!("Unknown node kind `{kind}` at {}", node.pos());
        }
    }
}

pub fn parse_num<T: Num>(text: &str) -> T
where
    T::FromStrRadixErr: Debug,
{
    if text.starts_with("0x") {
        T::from_str_radix(&text[2..], 16).unwrap()
    } else if text.starts_with("0b") {
        T::from_str_radix(&text[2..], 2).unwrap()
    } else if text.starts_with("0o") {
        T::from_str_radix(&text[2..], 8).unwrap()
    } else {
        T::from_str_radix(text, 10).unwrap()
    }
}

pub fn lower_literal<'a>(args: &Args, headers: &Headers, mut element: Literal<'a>) -> Literal<'a> {
    if args.emit_chars_literally {
        if let Literal::CharEscape(escape) = element {
            element = Literal::Char(match escape {
                '\'' => '\'',
                '\\' => '\\',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '0' => '\0',
                ch => unreachable!("Unknown escape sequence `\\{ch}`"),
            })
        }
    }
    if args.emit_chars_as_numbers {
        if let Literal::Char(ch) = element {
            element = Literal::Num((ch as u32).into())
        }
    }
    if let Literal::Num(ref n) = element {
        assert!(
            n.bits() <= headers.bits,
            "your literals don't fit within the given bits value. specifically, {n} requires too many bits"
        );
    }
    element
}
