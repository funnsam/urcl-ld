use core::fmt;
use std::collections::HashMap;
use std::io::{self, Write};

peg::parser! {
    pub grammar urcl() for str {
        rule _
            = quiet! {(
                "/*" ([^ '*'] / ("*" !"/"))* "*/"
                / "//" [^ '\n']*
                / [' ' | '\t']+
            )*};
        rule __
            = quiet! {(
                "/*" ([^ '*'] / ("*" !"/"))* "*/"
                / "//" [^ '\n']*
                / [' ' | '\n' | '\t']+
            )*};
        rule ___
            = ("\n" / quiet! { "\r\n" })+ / ![_];

        rule ident() -> &'input str
            = i:$(['_' | 'a'..='z' | 'A'..='Z']['_' | '.' | '0'..='9' | 'a'..='z' | 'A'..='Z']*) { i };
        rule ident_macro() -> &'input str
            = i:$("@"? ident()) { i };

        pub rule file() -> File<'input>
            = __ lines:line() ** (_ ___ __) __ { File { lines } };

        rule line() -> Line<'input>
            = inst:ident_macro() _ "[" __ opr:(operand() ** __) __ "]" { Line::Instruction(inst, opr) }
            / inst:ident_macro() _  opr:(operand() ** _) { Line::Instruction(inst, opr) }
            / ".." id:ident() { Line::LocalLabelDef(id) }
            / "." id:ident() { Line::LabelDef(id) }
            / "!" ex:ident() __ "." lc:ident() { Line::SymbolDef(ex, lc) };

        rule operand() -> Node<Operand<'input>>
            = s:position!() node:_operand() e:position!() { Node { node, span: s..e } };

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
    pub span: core::ops::Range<usize>,
}

#[derive(Debug, Clone)]
pub struct File<'a> {
    lines: Vec<Line<'a>>,
}

#[derive(Debug, Clone)]
pub enum Line<'a> {
    Instruction(&'a str, Vec<Node<Operand<'a>>>),
    SymbolDef(&'a str, &'a str),
    LocalLabelDef(&'a str),
    LabelDef(&'a str),
    LabelIdDef(usize),
}

#[derive(Debug, Clone)]
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

pub fn link_files<'a>(files: &mut [File<'a>]) {
    let mut alloc = IdAlloc::default();
    let mut symbols = HashMap::new();

    for f in files.iter() {
        for l in f.lines.iter() {
            if let Line::SymbolDef(sym, _) = l {
                symbols.entry(sym.to_string())
                    .and_modify(|_| panic!())
                    .or_insert(alloc.alloc());
            }
        }
    }

    let mut labels = HashMap::new();
    let mut loc_lbs = vec![HashMap::new()];

    for f in files.iter_mut() {
        labels.clear();
        if loc_lbs.len() != 1 { loc_lbs.drain(1..); }
        loc_lbs[0].clear();

        for l in f.lines.iter_mut() {
            match l {
                Line::LocalLabelDef(lb) => {
                    loc_lbs.last_mut().unwrap().entry(lb.to_string())
                        .and_modify(|_| panic!())
                        .or_insert(alloc.alloc());
                },
                Line::LabelDef(lb) => {
                    let id = *labels.entry(lb.to_string())
                        .and_modify(|_| panic!())
                        .or_insert(alloc.alloc());
                    *l = Line::LabelIdDef(id);
                    loc_lbs.push(HashMap::new());
                },
                Line::SymbolDef(sym, lb) => {
                    let id = *labels.entry(lb.to_string())
                        .and_modify(|_| panic!())
                        .or_insert(symbols[*sym]);
                    *l = Line::LabelIdDef(id);
                    loc_lbs.push(HashMap::new());
                },
                _ => {},
            }
        }

        let mut local_id = 0;
        for l in f.lines.iter_mut() {
            match l {
                Line::Instruction(_, ops) => for op in ops.iter_mut() {
                    match op.node {
                        Operand::LocalLabel(l) => op.node = Operand::IdLabel(loc_lbs[local_id][l]),
                        Operand::Label(l) => op.node = Operand::IdLabel(labels[l]),
                        Operand::Symbol(s) => op.node = Operand::IdLabel(symbols[s]),
                        _ => {},
                    }
                },
                Line::LabelDef(..) | Line::SymbolDef(..) | Line::LabelIdDef(..) => local_id += 1,
                Line::LocalLabelDef(lb) => {
                    *l = Line::LabelIdDef(loc_lbs[local_id][*lb]);
                },
            }
        }
    }
}

impl File<'_> {
    pub fn write_files(w: &mut impl Write, files: &[Self]) -> io::Result<()> {
        let mut after_dw = false;
        for f in files.iter() {
            for (i, l) in f.lines.iter().enumerate() {
                if i == 0 && after_dw && matches!(l, Line::SymbolDef(..) | Line::LabelDef(..) | Line::LabelIdDef(..)) {
                    writeln!(w, "nop")?;
                }

                match l {
                    Line::Instruction(inst, ops) if inst.eq_ignore_ascii_case("dw") && ops.len() > 1 => {
                        write!(w, "{inst} [")?;
                        for op in ops.iter() {
                            write!(w, " {}", op.node)?;
                        }
                        writeln!(w, " ]")?;

                        after_dw = true;
                        continue;
                    },
                    Line::Instruction(inst, ops) => {
                        write!(w, "{inst}")?;
                        for op in ops.iter() {
                            write!(w, " {}", op.node)?;
                        }
                        writeln!(w)?;
                    },

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
