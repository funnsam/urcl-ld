use core::fmt;
use std::collections::HashMap;
use std::io::{self, Write};

pub type Span = core::ops::Range<usize>;

peg::parser! {
    pub grammar urcl() for str {
        rule _
            = quiet! {(
                "/*" ([^ '*'] / ("*" !"/"))* "*/"
                / "//" [^ '\n']*
                / [' ' | '\t']+
            )+};
        rule __
            = quiet! {(
                "/*" ([^ '*'] / ("*" !"/"))* "*/"
                / "//" [^ '\n']*
                / [' ' | '\n' | '\t']+
            )*};

        rule ident() -> &'input str
            = i:$(['_' | 'a'..='z' | 'A'..='Z']['_' | '.' | '0'..='9' | 'a'..='z' | 'A'..='Z']*) { i };
        rule ident_macro() -> &'input str
            = i:$("@"? ident()) { i };

        pub rule file() -> File<'input>
            = __ lines:line() ** (_? (("\n" / quiet! { "\r\n" })+ / ![_]) __) __ { File { lines } };

        rule line() -> Node<Line<'input>>
            = s:position!() l:_line() e:position!() { Node { node: l, span: s..e } };
        rule _line() -> Line<'input>
            = "@" ['D' | 'd']['E' | 'e']['F' | 'f']['I' | 'i']['N' | 'n']['E' | 'e'] _ to:operand() _ from:operand() { Line::Define(to, from) }
            / inst:ident_macro() _ "[" __ opr:(operand() ** __) __ "]" { Line::Instruction(inst, opr) }
            / inst:ident_macro() _ opr:(operand() ** _) { Line::Instruction(inst, opr) }
            / inst:ident_macro() { Line::Instruction(inst, vec![]) }
            / ".." id:ident() { Line::LocalLabelDef(id) }
            / "." id:ident() { Line::LabelDef(id) }
            / "!" ex:ident() __ "." lc:ident() { Line::SymbolDef(ex, lc) };

        rule operand() -> Node<Operand<'input>>
            = s:position!() op:_operand() e:position!() { Node { node: op, span: s..e } };
        rule _operand() -> Operand<'input>
            = n:numeral() { Operand::Constant(n) }
            / ['r' | 'R' | '$'] i:$(['0'..='9']+) {? i.parse().map_or(Err("number too big"), |n| Ok(Operand::Register(n))) }
            / ['m' | 'M' | '#'] i:$(['0'..='9']+) {? i.parse().map_or(Err("number too big"), |n| Ok(Operand::Memory(n))) }
            / "\"" s:$(([^ '"'] / "\\\"")*) "\"" { Operand::String(s) }
            / "'" s:$(([^ '\''] / "\\\'")*) "'" { Operand::Character(s) }
            / "~" i:$(['+' | '-']? ['0'..='9']+) {? i.parse().map_or(Err("number too big"), |n| Ok(Operand::Relative(n))) }
            / "%" id:ident() { Operand::Port(id) }
            / "!" id:ident() { Operand::Symbol(id) }
            / ".." id:ident() { Operand::LocalLabel(id) }
            / "." id:ident() { Operand::Label(id) }
            / id:ident_macro() { Operand::Ident(id) };

        rule numeral() -> i128
            = "0x" i:$(['0'..='9' | 'a'..='f' | 'A'..='F']+) {? i128::from_str_radix(i, 16).map_err(|_| "number too big") }
            / "0o" i:$(['0'..='7']+) {? i128::from_str_radix(i, 8).map_err(|_| "number too big") }
            / "0b" i:$(['0'..='1']+) {? i128::from_str_radix(i, 2).map_err(|_| "number too big") }
            / i:$(['+' | '-']? ['0'..='9']+) {? i.parse().map_err(|_| "number too big") };
    }
}

#[derive(Debug, Clone)]
pub struct Node<T> {
    pub node: T,
    pub span: Span,
}

impl<T: fmt::Display> fmt::Display for Node<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.node.fmt(f) }
}

#[derive(Debug, Clone)]
pub struct File<'a> {
    lines: Vec<Node<Line<'a>>>,
}

#[derive(Debug, Clone)]
pub enum Line<'a> {
    Instruction(&'a str, Vec<Node<Operand<'a>>>),
    Define(Node<Operand<'a>>, Node<Operand<'a>>),
    SymbolDef(&'a str, &'a str),
    LocalLabelDef(&'a str),
    LabelDef(&'a str),
    LabelIdDef(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand<'a> {
    Constant(i128),
    Register(usize),
    Memory(usize),
    String(&'a str),
    Character(&'a str),
    Relative(i128),
    Port(&'a str),
    Symbol(&'a str),
    LocalLabel(&'a str),
    Label(&'a str),
    Ident(&'a str),

    IdLabel(usize),
}

#[derive(Default, Debug)]
struct IdAlloc(usize);

impl IdAlloc {
    fn alloc(&mut self) -> usize {
        self.0 += 1;
        self.0 - 1
    }
}

pub fn link_files<'a>(files: &mut [File<'a>]) -> Result<(), LinkError> {
    let mut alloc = IdAlloc::default();
    let mut symbols = HashMap::new();

    for (fi, f) in files.iter().enumerate() {
        for l in f.lines.iter() {
            if let Line::SymbolDef(sym, _) = l.node {
                if symbols.contains_key(sym) { return Err(LinkErrorType::DuplicatedSymbol.full(fi, l.span.clone())); }
                symbols.insert(sym.to_string(), alloc.alloc());
            }
        }
    }

    let mut defines = HashMap::new();
    let mut labels = HashMap::new();
    let mut loc_lbs = vec![HashMap::new()];
    let mut remove = Vec::new();

    for (fi, f) in files.iter_mut().enumerate() {
        defines.clear();
        labels.clear();
        if loc_lbs.len() != 1 { loc_lbs.drain(1..); }
        loc_lbs[0].clear();

        for l in f.lines.iter_mut() {
            match &l.node {
                Line::LocalLabelDef(lb) => {
                    if loc_lbs.last().unwrap().contains_key(*lb) { return Err(LinkErrorType::DuplicatedLocalLabel.full(fi, l.span.clone())); }

                    loc_lbs.last_mut().unwrap().insert(lb.to_string(), alloc.alloc());
                },
                Line::LabelDef(lb) => {
                    if labels.contains_key(*lb) { return Err(LinkErrorType::DuplicatedLabel.full(fi, l.span.clone())); }

                    let id = *labels.entry(lb.to_string())
                        .or_insert(alloc.alloc());
                    l.node = Line::LabelIdDef(id);
                    loc_lbs.push(HashMap::new());
                },
                Line::SymbolDef(sym, lb) => {
                    if labels.contains_key(*lb) { return Err(LinkErrorType::DuplicatedLabel.full(fi, l.span.clone())); }

                    let id = *labels.entry(lb.to_string())
                        .or_insert(symbols[*sym]);
                    l.node = Line::LabelIdDef(id);
                    loc_lbs.push(HashMap::new());
                },
                Line::Define(from, to) => {
                    if defines.contains_key(&from.node) { return Err(LinkErrorType::DuplicatedDefine.full(fi, l.span.clone())); }

                    defines.insert(from.node.clone(), to.clone());
                },
                _ => {},
            }
        }

        let mut local_id = 0;
        remove.clear();

        for (li, l) in f.lines.iter_mut().enumerate() {
            match &mut l.node {
                Line::Instruction(_, ops) => for op in ops.iter_mut() {
                    op.node = resolve_operand(
                        op.clone(),
                        &symbols,
                        &labels,
                        &loc_lbs[local_id],
                        &defines,
                    ).map_err(|e| e.full(fi, op.span.clone()))?;
                },
                Line::LabelDef(..) | Line::SymbolDef(..) | Line::LabelIdDef(..) => local_id += 1,
                Line::LocalLabelDef(lb) => {
                    l.node = Line::LabelIdDef(loc_lbs[local_id][*lb]);
                },
                Line::Define(..) => remove.push(li),
            }
        }

        for r in remove.iter().rev() {
            f.lines.remove(*r);
        }
    }

    Ok(())
}

fn resolve_operand<'a>(
    op: Node<Operand<'a>>,
    symbols: &HashMap<String, usize>,
    labels: &HashMap<String, usize>,
    loc_lbs: &HashMap<String, usize>,
    defines: &HashMap<Operand<'a>, Node<Operand<'a>>>,
) -> Result<Operand<'a>, LinkErrorType> {
    if let Some(op) = defines.get(&op.node) {
        return resolve_operand(op.clone(), symbols, labels, loc_lbs, defines);
    }

    match op.node {
        Operand::LocalLabel(l)
            => Ok(Operand::IdLabel(*loc_lbs.get(l).ok_or(LinkErrorType::UnknownLocalLabel)?)),
        Operand::Label(l)
            => Ok(Operand::IdLabel(*labels.get(l).ok_or(LinkErrorType::UnknownLabel)?)),
        Operand::Symbol(s)
            => Ok(Operand::IdLabel(*symbols.get(s).ok_or(LinkErrorType::UnknownSymbol)?)),
        Operand::Ident(_)
            => Err(LinkErrorType::UnknownIdent),
        _ => Ok(op.node),
    }
}

impl File<'_> {
    pub fn write_files(w: &mut impl Write, files: &[Self]) -> io::Result<()> {
        let mut after_dw = false;
        for f in files.iter() {
            for (i, l) in f.lines.iter().enumerate() {
                if i == 0 && after_dw && matches!(l.node, Line::SymbolDef(..) | Line::LabelDef(..) | Line::LocalLabelDef(..) | Line::LabelIdDef(..)) {
                    writeln!(w, "nop")?;
                }

                match &l.node {
                    Line::Instruction(inst, ops) if inst.eq_ignore_ascii_case("dw") && ops.len() > 1 => {
                        write!(w, "{inst} [")?;
                        for op in ops.iter() {
                            write!(w, " {op}")?;
                        }
                        writeln!(w, " ]")?;

                        after_dw = true;
                        continue;
                    },
                    Line::Instruction(inst, ops) => {
                        write!(w, "{inst}")?;
                        for op in ops.iter() {
                            write!(w, " {op}")?;
                        }
                        writeln!(w)?;
                    },

                    Line::Define(from, to) => writeln!(w, "@define {from} {to}")?,

                    Line::LocalLabelDef(lb) => writeln!(w, "..{lb}")?,
                    Line::LabelDef(lb) => writeln!(w, ".{lb}")?,
                    Line::SymbolDef(sym, lb) => writeln!(w, "!{sym} .{lb}")?,

                    Line::LabelIdDef(id) => writeln!(w, "._{id}")?,
                }

                after_dw = false;
            }
        }

        Ok(())
    }
}

impl fmt::Display for Operand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Constant(n) => write!(f, "{n}"),
            Self::Register(r) => write!(f, "r{r}"),
            Self::Memory(m) => write!(f, "m{m}"),
            Self::String(s) => write!(f, "\"{s}\""),
            Self::Character(c) => write!(f, "'{c}'"),
            Self::Relative(r) => write!(f, "~{r:+}"),
            Self::Port(p) => write!(f, "%{p}"),
            Self::Symbol(s) => write!(f, "!{s}"),
            Self::LocalLabel(l) => write!(f, "..{l}"),
            Self::Label(l) => write!(f, ".{l}"),
            Self::Ident(i) => write!(f, "{i}"),
            Self::IdLabel(i) => write!(f, "._{i}"),
        }
    }
}

#[derive(Debug)]
pub struct LinkError {
    pub typ: LinkErrorType,
    pub file: usize,
    pub span: Span,
}

#[derive(thiserror::Error, Debug)]
pub enum LinkErrorType {
    #[error("duplicated symbol")]
    DuplicatedSymbol,
    #[error("duplicated label")]
    DuplicatedLabel,
    #[error("duplicated local label")]
    DuplicatedLocalLabel,
    #[error("duplicated definition")]
    DuplicatedDefine,

    #[error("unknown symbol")]
    UnknownSymbol,
    #[error("unknown label")]
    UnknownLabel,
    #[error("unknown local label")]
    UnknownLocalLabel,
    #[error("unknown identifier")]
    UnknownIdent,
}

impl LinkErrorType {
    fn full(self, file: usize, span: Span) -> LinkError {
        LinkError { typ: self, file, span }
    }
}
