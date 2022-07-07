use super::*;
use num::BigUint;
use std::fmt::{self, Display, Formatter, Result};

pub struct SourceError<'a> {
    pub pos: Option<Position<'a>>,
    pub message: String,
}

#[macro_export]
macro_rules! stack {
    ($in:expr; -> $out:expr) => {
        StackBehaviour {
            input: $in,
            output: $out,
        }
    };
}

#[macro_export]
macro_rules! err {
    (@nopush None, $($arg:tt)*) => {
        err!(@ None, $($arg)*)
    };
    (@nopush $unit:expr, $node:expr, $($arg:tt)*) => {
        err!(@ Some($node.pos($unit)), $($arg)*)
    };
    (@ $pos:expr, $($t:tt)*) => {
        SourceError {
            pos: $pos,
            message: format!($($t)*),
        }
    };
    ($errors:expr; None$(; $value:expr)?, $($t:tt)*) => {{
        $errors.push(err!(@nopush None, $($t)*));
        $($value)?
    }};
    ($errors:expr; $unit:expr; $node:expr$(; $value:expr)?, $($t:tt)*) => {{
        $errors.push(err!(@nopush $unit, $node, $($t)*));
        $($value)?
    }};
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

#[derive(Clone)]
pub struct Position<'a> {
    pub unit: &'a CompilationUnit<'a>,
    pub range: tree_sitter::Range,
}

impl Display for Position<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}:{}:{}",
            self.unit.path,
            self.range.start_point.row + 1,
            self.range.start_point.column + 1
        )
    }
}

pub struct Function<'a> {
    pub node: Node<'a>,
    pub name: &'a str,
    pub stack: StackBehaviour,
    pub unit: &'a CompilationUnit<'a>,
    pub body: FunctionBody<'a>,
    pub pos: Position<'a>,
}

pub enum FunctionBody<'a> {
    Ursl {
        locals: usize,
        instructions: Vec<ursl::InstructionEntry<'a>>,
    },
    Urcl {
        overloads: Vec<UrclMainBody<'a>>,
        branch: Option<UrclBranchBody<'a>>,
    },
    Permutation(Permutation),
}

pub struct UrclMainBody<'a> {
    pub input: urcl::StackBindings<'a>,
    pub output: urcl::StackBindings<'a>,
    pub instructions: Vec<urcl::InstructionEntry<'a>>,
    pub pos: Position<'a>,
}

pub struct UrclBranchBody<'a> {
    pub input: urcl::StackBindings<'a>,
    pub instructions: Vec<urcl::InstructionEntry<'a>>,
    pub pos: Position<'a>,
}

pub trait PositionEntry {
    fn pos(&self) -> Position;
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

pub fn parse_literal<'a>(node: Node<'a>, unit: &'a CompilationUnit<'a>) -> Literal<'a> {
    match node.kind() {
        "char_literal" => {
            let char_literal_value = node.field("value", unit);
            match char_literal_value.kind() {
                "char" => {
                    let char_text = char_literal_value.text(unit);
                    if let &[ch] = char_text.chars().collect::<Vec<_>>().as_slice() {
                        Literal::Char(ch)
                    } else {
                        unreachable!("Char literal should only contain 1 char {}", node.pos(unit));
                    }
                }
                "char_escape" => {
                    let char_text = char_literal_value.text(unit);
                    if let &[escape, ch] = char_text.chars().collect::<Vec<_>>().as_slice() {
                        assert_eq!(escape, '\\');
                        Literal::CharEscape(ch)
                    } else {
                        unreachable!(
                            "Escape sequence should always be 2 chars at {}",
                            node.pos(unit)
                        );
                    }
                }
                _ => unknown_node(node, unit),
            }
        }
        "number" => Literal::Num(parse_num(node.text(unit))),
        "macro" => Literal::Macro(node.field("name", unit).text(unit)),
        "data_label" => Literal::Label(node.field("name", unit).text(unit)),
        "function_name" => Literal::Func(node.text(unit)),
        "mem" => Literal::Mem(node.field("index", unit).text(unit).parse().unwrap()),
        _ => unknown_node(node, unit),
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

#[cold]
pub fn unknown_node(node: Node, unit: &CompilationUnit) -> ! {
    unreachable!("Unknown node kind `{}` at {}", node.kind(), node.pos(unit))
}

pub trait SourceErrors<'a> {
    type T;
    fn extend_into(self, errs: &mut Vec<SourceError<'a>>) -> Self::T;
}

impl<'a, T> SourceErrors<'a> for (T, Vec<SourceError<'a>>) {
    type T = T;
    fn extend_into(self, errs: &mut Vec<SourceError<'a>>) -> Self::T {
        let (value, errors) = self;
        errs.extend(errors);
        value
    }
}

#[derive(Clone)]
pub struct RegisterAllocation<'a>(Vec<AllocationSlot<'a>>);

#[derive(Clone)]
pub enum AllocationSlot<'a> {
    Register(usize),
    Literal(Literal<'a>),
}

impl Display for AllocationSlot<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Register(reg) => write!(f, "${reg}"),
            Self::Literal(lit) => write!(f, "{lit}"),
        }
    }
}

impl<'a> RegisterAllocation<'a> {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn normal(height: usize) -> Self {
        Self((1..=height).map(AllocationSlot::Register).collect())
    }

    pub fn offset(&mut self, offset: usize) {
        for reg in self.0.iter_mut() {
            if let AllocationSlot::Register(reg) = reg {
                *reg += offset;
            }
        }
    }

    pub fn pop(&mut self, length: usize) {
        for _ in 0..length {
            self.0.pop();
        }
    }

    pub fn push(&mut self, new: AllocationSlot<'a>) {
        self.0.push(new);
    }

    pub fn all_used_regs(&self) -> Vec<usize> {
        let mut used = Vec::with_capacity(self.0.len());
        for i in self.0.iter() {
            if let AllocationSlot::Register(i) = i {
                if !used.contains(i) {
                    used.push(*i);
                }
            }
        }
        used
    }

    pub fn get(&self, length: usize) -> &[AllocationSlot<'a>] {
        &self.0[(self.0.len() - length)..]
    }

    pub fn top(&self) -> AllocationSlot<'a> {
        self.0
            .last()
            .cloned()
            .unwrap_or(AllocationSlot::Register(0))
    }

    pub fn is_reg_unique(&self, reg: usize) -> bool {
        self.0
            .iter()
            .filter(|r| match r {
                AllocationSlot::Register(r) => *r == reg,
                AllocationSlot::Literal(_) => false,
            })
            .count()
            == 1
    }

    pub fn next_reg(&self) -> AllocationSlot<'a> {
        // pretty sure this is O(n^2) and can be improved, but i have no idea how to do so.
        // (the difficult part is reusing old regs, so max value is no good)
        for i in 1.. {
            // bullshit impl of contains that does not rely on an impl PartialEq for Literal
            if self
                .0
                .iter()
                .filter_map(|r| {
                    if let AllocationSlot::Register(r) = r {
                        Some(*r)
                    } else {
                        None
                    }
                })
                .find(|&r| r == i)
                .is_none()
            {
                return AllocationSlot::Register(i);
            }
        }

        panic!(
            "Holy shit, you used all {} registers! Congrats? what the hell is wrong with you.",
            usize::MAX
        );
    }

    pub fn apply_next_reg(&mut self) -> AllocationSlot<'a> {
        let reg = self.next_reg();
        self.push(reg.clone());
        reg
    }

    pub fn apply_pop1(&mut self) -> AllocationSlot<'a> {
        let reg = self.top();
        self.pop(1);
        reg
    }

    pub fn apply_permutation(&mut self, perm: &Permutation) {
        let inputs = self.get(perm.input).to_vec();
        self.pop(inputs.len());
        for &i in perm.output.iter() {
            self.push(inputs[i].clone());
        }
    }

    pub fn normalize(
        &mut self,
        args: &Args,
        f: &mut impl Write,
        max_regs: &mut usize,
        top_literals_unchanged: usize,
    ) -> io::Result<()> {
        let length = self.0.len() - top_literals_unchanged;
        let mut changes = self
            .0
            .iter()
            .enumerate()
            .take(length)
            .filter_map(|(dest, slot)| {
                if let AllocationSlot::Register(src) = slot {
                    Some((*src, dest + 1))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        let mut ignored_on_purpose = Vec::new();
        let top_changes = self
            .0
            .iter_mut()
            .skip(length)
            .enumerate()
            .filter_map(|(i, slot)| {
                if let AllocationSlot::Register(src) = slot {
                    // if the register is already used in the pre-shuffle part, just assign it to the final destination and don't touch it here!
                    // note that it does not fucking matter which dest is chosen on the off chance that the find will be valid for several items
                    if let Some((_, dest)) = changes.iter().find(|(s, _)| *s == *src) {
                        ignored_on_purpose.push(i + 1);
                        *src = *dest;
                        None
                    } else {
                        Some((*src, i + length + 1))
                    }
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        changes = changes.into_iter().filter(|(src, dest)| src != dest).collect();
        let circular_temp_reg = length + top_changes.len() + 1;
        changes.extend(top_changes);
        let literals = self
            .0
            .iter()
            .enumerate()
            .take(length)
            .filter_map(|(dest, slot)| {
                if let AllocationSlot::Literal(lit) = slot {
                    Some((lit, dest + 1))
                } else {
                    None
                }
            });
        if args.verbose {
            println!("normalize:{self:?}");
            for &(src, dest) in changes.iter() {
                println!("{src} -> {dest}");
            }
        }
        // Don't use ? operator so that it still finishes all calculations even if writing fails.
        let mut result = Ok(());
        if args.verbose {
            result = result.and_then(|()| writeln!(f, "// prev alloc:{self:?}"));
        }
        // Changes that are written to a register that no other changes will read from. They are safe to do immediately.
        while let Some(dangling) = changes
            .iter()
            .position(|&(_, dest)| !changes.iter().any(|&(src, _)| src == dest))
        {
            let (src, dest) = changes.swap_remove(dangling);
            *max_regs = (*max_regs).max(src).max(dest);
            result = result.and_then(|()| writeln!(f, "MOV ${} ${}", dest, src));
        }
        while let Some((first_src, mut last_dest)) = changes.pop() {
            let mut circular = vec![circular_temp_reg, first_src];
            while let Some(i) = changes.iter().position(|&(src, _)| src == last_dest) {
                let (_, dest) = changes.swap_remove(i);
                circular.push(last_dest);
                last_dest = dest;
            }
            circular.push(circular_temp_reg);

            for i in 1..circular.len() {
                let src = circular[i];
                let dest = circular[i - 1];
                *max_regs = (*max_regs).max(src).max(dest);
                result = result.and_then(|()| writeln!(f, "MOV ${} ${}", dest, src));
            }
        }

        for (src, dest) in literals {
            *max_regs = (*max_regs).max(dest);
            result = result.and_then(|()| writeln!(f, "IMM ${} {}", dest, src));
        }

        for i in 0..length {
            self.0[i] = AllocationSlot::Register(i + 1);
        }
        for i in length..self.0.len() {
            if ignored_on_purpose.contains(&i) {
                continue
            }
            // after `length`, literals are not normalized
            if let AllocationSlot::Register(_) = self.0[i] {
                self.0[i] = AllocationSlot::Register(i + 1);
            }
        }
        if args.verbose {
            result = result.and_then(|()| writeln!(f, "// new alloc:{self:?}"));
        }
        result
    }
}

impl Debug for RegisterAllocation<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0.iter().fold(Ok(()), |result, slot| {
            result.and_then(|()| write!(f, " {slot}"))
        })
    }
}
