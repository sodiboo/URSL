use super::*;
use num::BigUint;
use std::fmt::{self, Display, Formatter};

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

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct StackBehaviour {
    pub input: usize,
    pub output: usize,
}

impl Display for StackBehaviour {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.input, self.output)
    }
}

#[derive(Clone)]
pub struct Position<'a> {
    pub unit: &'a CompilationUnit<'a>,
    pub range: tree_sitter::Range,
}

impl Display for Position<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    Extern(CallingConvention, Cow<'a, str>),
    Deferred,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CallingConvention {
    URSL,
    URCLpp,
    Hexagn,
}

impl Display for CallingConvention {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CallingConvention::URSL => write!(f, "URSL"),
            CallingConvention::URCLpp => write!(f, "URCL++"),
            CallingConvention::Hexagn => write!(f, "Hexagn"),
        }
    }
}

pub struct UrclMainBody<'a> {
    pub input: urcl::InputStackBindings<'a>,
    pub output: urcl::OutputStackBindings<'a>,
    pub instructions: Vec<urcl::InstructionEntry<'a>>,
    pub pos: Position<'a>,
}

pub struct UrclBranchBody<'a> {
    pub input: urcl::InputStackBindings<'a>,
    pub instructions: Vec<urcl::InstructionEntry<'a>>,
    pub pos: Position<'a>,
}

pub trait PositionEntry {
    fn pos(&self) -> Position;
}

#[derive(Clone)]
pub enum DataLiteral<'a> {
    Literal(Literal<'a>),
    Array(Vec<(Node<'a>, DataLiteral<'a>)>),
    String(SyntaxString<'a>),
}

impl Display for DataLiteral<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(element) => element.fmt(f),
            Self::Array(elements) => {
                write!(f, "[ ")?;
                for (_, element) in elements {
                    write!(f, "{element} ")?;
                }
                write!(f, "]")
            }
            Self::String(string) => write!(f, "{string:?}"),
        }
    }
}

pub fn parse_data_literal<'a>(
    node: Node<'a>,
    unit: &'a CompilationUnit<'a>,
) -> (DataLiteral<'a>, Vec<SourceError<'a>>) {
    let mut errors = Vec::new();
    let result = match node.kind() {
        "array" => DataLiteral::Array(
            node.children_by_field_name("item", &mut unit.tree.walk())
                .map(|node| {
                    (
                        node,
                        parse_data_literal(node, unit).extend_into(&mut errors),
                    )
                })
                .collect(),
        ),
        "string" => DataLiteral::String(parse_string(node, unit).extend_into(&mut errors)),
        _ => DataLiteral::Literal(parse_literal(node, unit).extend_into(&mut errors)),
    };
    (result, errors)
}

#[derive(Clone)]
pub enum Literal<'a> {
    Char(char),
    CharEscape(CharEscape),
    Macro(&'a str),
    Num(BigUint),
    Mem(u64),
    Label(&'a str),
    Func(&'a str),
}

#[derive(Clone, Copy)]
pub enum CharEscape {
    Hex(u8),
    Unicode(u32),
    Char(char),
}

impl Into<char> for CharEscape {
    fn into(self) -> char {
        match self {
            Self::Hex(hex) => hex.into(),
            Self::Unicode(unicode) => unicode.try_into().unwrap_or('\0'),
            Self::Char(char) => char,
        }
    }
}

impl Display for CharEscape {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Hex(v) => write!(f, "\\x{v:02x}"),
            Self::Unicode(v) => {
                if v < u16::MAX as _ {
                    write!(f, "\\u{v:04x}")
                } else {
                    write!(f, "\\U{v:08x}")
                }
            }
            Self::Char(v) => write!(f, "\\{v}"),
        }
    }
}

impl Display for Literal<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Char(ch) => write!(f, "'{ch}'"),
            Self::CharEscape(esc) => write!(f, "'{esc}'"),
            Self::Macro(name) => write!(f, "@{name}"),
            Self::Num(n) => write!(f, "{n}"),
            Self::Mem(addr) => write!(f, "#{addr}"),
            Self::Label(name) => write!(f, ".{}", mangle::data_label(name)),
            Self::Func(name) => write!(f, ".{}", mangle::function_name(name)),
        }
    }
}

pub fn parse_literal<'a>(
    node: Node<'a>,
    unit: &'a CompilationUnit<'a>,
) -> (Literal<'a>, Vec<SourceError<'a>>) {
    let mut errors = Vec::new();
    let literal = match node.kind() {
        "char" => {
            let node = node.field("content", unit);
            match node.kind() {
                "char_value" => Literal::Char(node.text(unit).chars().next().unwrap_or_else(|| {
                    err!(errors; unit; node, "Somehow the char literal is empty. What the fuck.");
                    '\0'
                })),
                "escape_sequence" => {
                    Literal::CharEscape(parse_char_escape(node, unit).extend_into(&mut errors))
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
    };
    (literal, errors)
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

pub fn parse_char_escape<'a>(
    node: Node<'a>,
    unit: &'a CompilationUnit<'a>,
) -> (CharEscape, Vec<SourceError<'a>>) {
    let (parent, node) = (node, node.field("value", unit));
    let mut errors = Vec::new();
    let esc = match node.kind() {
        "hex_escape" => {
            CharEscape::Hex(u8::from_str_radix(node.text(unit), 16).unwrap_or_else(|_| {
                err!(errors; unit; parent, "Invalid hex literal");
                0
            }))
        }
        "unicode_escape" => CharEscape::Unicode({
            let u = u32::from_str_radix(node.text(unit), 16).unwrap_or_else(|_| {
                err!(errors; unit; parent, "Invalid hex literal");
                0
            });
            if let Err(_) = char::try_from(u) {
                err!(errors; unit; parent, "Not a unicode codepoint");
            }
            u
        }),
        "char_escape" => CharEscape::Char(node.text(unit).chars().next().unwrap_or_else(|| {
            err!(errors; unit; parent, "Somehow the char escape is empty. What the fuck.");
            '\0'
        })),
        _ => unknown_node(node, unit),
    };
    (esc, errors)
}

pub fn lower_char_escape(ch: CharEscape) -> Option<char> {
    match ch {
        CharEscape::Char('\'') => Some('\''),
        CharEscape::Char('\\') => Some('\\'),
        CharEscape::Char('n') => Some('\n'),
        CharEscape::Char('r') => Some('\r'),
        CharEscape::Char('t') => Some('\t'),
        CharEscape::Char('0') => Some('\0'),
        CharEscape::Char(_) => None,
        CharEscape::Hex(x) => Some(x.into()),
        CharEscape::Unicode(x) => x.try_into().ok(),
    }
}

pub fn lower_literal<'a>(
    args: &Args,
    headers: &Headers,
    mut element: Literal<'a>,
    node: Node<'a>,
    unit: &'a CompilationUnit<'a>,
) -> (Literal<'a>, Vec<SourceError<'a>>) {
    let mut errors = Vec::new();
    if args.emit_chars_literally {
        if let Literal::CharEscape(escape) = element {
            element = Literal::Char(lower_char_escape(escape).unwrap_or_else(|| {
                err!(errors; unit; node, "Invalid char escape: {}", escape);
                '\0'
            }));
        }
    }
    if args.emit_chars_as_numbers {
        if let Literal::Char(ch) = element {
            element = Literal::Num((ch as u32).into())
        }
    }
    if let Literal::Num(ref n) = element {
        if n.bits() > headers.bits {
            err!(errors; unit; node, "This literal requires {} bits, but the bits header is set to {}.", n.bits(), headers.bits);
        }
    }
    (element, errors)
}

pub fn lower_data_literal<'a>(
    args: &Args,
    headers: &Headers,
    mut element: DataLiteral<'a>,
    node: Node<'a>,
    unit: &'a CompilationUnit<'a>,
) -> (DataLiteral<'a>, Vec<SourceError<'a>>) {
    let mut errors = Vec::new();

    if args.emit_strings_as_chars {
        if let DataLiteral::String(string) = element {
            let mut chars = Vec::new();
            for segment in string.into_segments() {
                match segment {
                    StringSegment::Literal(segment) => {
                        for ch in segment.chars() {
                            chars.push((node, DataLiteral::Literal(Literal::Char(ch))));
                        }
                    }
                    StringSegment::Escape(esc) => {
                        chars.push((node, DataLiteral::Literal(Literal::CharEscape(esc))));
                    }
                }
            }
            element = DataLiteral::Array(chars);
        }
    }

    if let DataLiteral::Array(items) = element {
        let mut result = Vec::new();
        for (_, item) in items.into_iter().map(|(node, item)| {
            (
                node,
                lower_data_literal(args, headers, item, node, unit).extend_into(&mut errors),
            )
        }) {
            if args.flatten_arrays {
                if let DataLiteral::Array(items) = item {
                    result.extend(items);
                    continue;
                }
            }
            result.push((node, item));
        }
        element = DataLiteral::Array(result);
    }

    if let DataLiteral::Literal(literal) = element {
        let literal = lower_literal(args, headers, literal, node, unit).extend_into(&mut errors);
        element = DataLiteral::Literal(literal);
    }

    (element, errors)
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

    pub fn offset_except(&mut self, offset: usize, dont_offset: usize) {
        for reg in self.0.iter_mut() {
            if let AllocationSlot::Register(reg) = reg {
                if *reg != dont_offset {
                    *reg += offset;
                }
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

    pub fn can_own_in_place(&self, reg: usize) -> bool {
        reg != 0
            && self
                .0
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
                        ignored_on_purpose.push(i + length);
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
        changes = changes
            .into_iter()
            .filter(|(src, dest)| src != dest)
            .collect();
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
                continue;
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

#[derive(Clone, Copy)]
enum StringSegment<'a> {
    Literal(&'a str),
    Escape(CharEscape),
}

#[derive(Clone)]
pub struct SyntaxString<'a>(Vec<(Node<'a>, StringSegment<'a>)>);

impl Display for SyntaxString<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0
            .iter()
            .cloned()
            .fold(Ok(()), |result, (_node, segment)| {
                result.and_then(|()| match segment {
                    StringSegment::Literal(lit) => write!(f, "{lit}"),
                    StringSegment::Escape(esc) => write!(f, "{}", <_ as Into<char>>::into(esc)),
                })
            })
    }
}

impl Debug for SyntaxString<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "\"")?;
        self.0
            .iter()
            .cloned()
            .fold(Ok(()), |result, (_node, segment)| {
                result.and_then(|()| match segment {
                    StringSegment::Literal(lit) => write!(f, "{lit}"),
                    StringSegment::Escape(esc) => write!(f, "{esc}"),
                })
            })?;
        write!(f, "\"")
    }
}

impl<'a> SyntaxString<'a> {
    fn into_segments(self) -> impl Iterator<Item = StringSegment<'a>> {
        self.0.into_iter().map(|(_, segment)| segment)
    }
}

pub fn parse_string<'a>(
    node: Node<'a>,
    unit: &'a CompilationUnit<'a>,
) -> (SyntaxString<'a>, Vec<SourceError<'a>>) {
    let mut errors = Vec::new();
    let segments = node
        .children_by_field_name("content", &mut unit.tree.walk())
        .map(|node| {
            (
                node,
                match node.kind() {
                    "string_segment" => StringSegment::Literal(node.text(unit)),
                    "escape_sequence" => StringSegment::Escape(
                        parse_char_escape(node, unit).extend_into(&mut errors),
                    ),
                    _ => unknown_node(node, unit),
                },
            )
        })
        .collect();
    (SyntaxString(segments), errors)
}

pub fn parse_call_convention<'a>(
    node: Node<'a>,
    unit: &'a CompilationUnit<'a>,
) -> (CallingConvention, Vec<SourceError<'a>>) {
    let mut errors = Vec::new();
    let convention = match parse_string(node, unit)
        .extend_into(&mut errors)
        .to_string()
        .as_str()
    {
        "URSL" => CallingConvention::URSL,
        "URCL++" => CallingConvention::URCLpp,
        "Hexagn" => CallingConvention::Hexagn,
        unknown => match unknown.to_lowercase().as_str() {
            "ursl" => err!(
                errors; unit; node; CallingConvention::URSL,
                "Unknown calling convention: \"{unknown}\" (did you mean \"URSL\"?)"
            ),
            "urcl++" | "urclpp" => err!(
                errors; unit; node; CallingConvention::URCLpp,
                "Unknown calling convention: \"{unknown}\" (did you mean \"URCL++\"?)"
            ),
            "hexagn" => err!(
                errors; unit; node; CallingConvention::Hexagn,
                "Unknown calling convention: \"{unknown}\" (did you mean \"Hexagn\"?)"
            ),
            _ => {
                err!(errors; unit; node; CallingConvention::URSL, "Unknown calling convention: \"{unknown}\"")
            }
        },
    };
    (convention, errors)
}
