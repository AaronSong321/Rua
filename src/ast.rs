use std::fmt::{Debug, Write, Display};
use crate::calc::{self};

#[derive(Debug)]
pub enum Expr<'a> {
    Num(i32),
    Op(Box<Expr<'a>>, Opcode, Box<Expr<'a>>),
    Paren(Box<Expr<'a>>),
    Id(&'a str),
    Call(Box<Expr<'a>>, Vec<Expr<'a>>),
    ErrorExpr,
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(a) => f.write_fmt(format_args!("{:?}", a)),
            Self::Op(lhs, op, rhs) => f.write_fmt(format_args!("{} {} {}", *lhs, op, *rhs)),
            Self::Paren(inner) => f.write_fmt(format_args!("({})", *inner)),
            Self::ErrorExpr => f.write_str("<error-expr>"),
            Self::Call(f1, args) => {
                write!(f, "{}(", f1).unwrap();
                for i in 0..args.len() {
                    write!(f, "{}{}", args[i], if i == args.len()-1 { ", " } else { "" }).unwrap();
                }
                Ok(())
            }
            Self::Id(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Debug)]
pub enum Opcode {
    Add,
    Mul,
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Opcode::*;
        match self {
            Add => f.write_char('+'),
            Mul => f.write_char('*'),
        }
    }
}

#[derive(Debug)]
pub struct Chunk<'a> {
    pub v: Vec<Smt<'a>>
}

impl<'a> Chunk<'a> {
    pub fn new(v: Vec<Smt<'a>>) -> Self {
        Chunk {
            v
        }
    }
}

impl<'a> Display for Chunk<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..(self.v.len() - 1) {
            write!(f, "{};\n", self.v[i]).unwrap();
        }
        write!(f, "{};", self.v[self.v.len() - 1])
    }
}


#[derive(Debug)]
pub struct LocalVar<'a> {
    pub id: &'a str
}

impl<'a> Display for LocalVar<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}


#[derive(Debug)]
pub enum Smt<'a> {
    LocalVarDecl(Vec<LocalVar<'a>>, Option<Vec<Expr<'a>>>)
}

impl<'a> Display for Smt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Smt::LocalVarDecl(vars, exprs) => {
                write!(f, "local ").unwrap();
                for i in 0..(vars.len()-1){
                    write!(f, "{}, ", vars[i].id).unwrap();
                }
                write!(f, "{}", vars[vars.len() - 1].id).unwrap();
                if let Some(rexprs) = exprs {
                    write!(f, " = ").unwrap();
                    for i in 0..(rexprs.len() - 1){
                        write!(f, "{}, ", rexprs[i]).unwrap();
                    }
                    write!(f, "{}", rexprs[rexprs.len() - 1]).unwrap();
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct FunExpr<'a> {
    name: &'a str,
    params: Vec<LocalVar<'a>>,
    body: Chunk<'a>,
}

impl<'a> FunExpr<'a> {
    pub fn new(name: &'a str, params: Vec<LocalVar<'a>>, body: Chunk<'a>) -> Self {
        FunExpr {name, params, body}
    }
}

impl<'a> Display for FunExpr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function {}(", self.name).unwrap();
        if self.params.len() > 0 {
            for i in 0..(self.params.len() - 1) {
                write!(f, "{}, ", self.params[i]).unwrap();
            }
            write!(f, "{}", self.params[self.params.len() - 1]).unwrap();
        }
        write!(f, ")\n{}{}", self.body, if self.body.v.len() > 0 { "\nend" } else { "end" }).unwrap();
        Ok(())
    }
}


#[test]
fn expr() {
    let pars = calc::ExprParser::new();
    let mut diags = Vec::new();
    assert!(pars.parse(&mut diags, "22").is_ok());
    assert!(pars.parse(&mut diags, "((((22))))").is_ok());
    assert_eq!(&format!("{}", pars.parse(&mut diags, "22 * 44 + 66").unwrap()), "22 * 44 + 66");
    assert_eq!(&format!("{}", pars.parse(&mut diags, "22 *    (((44+66)))").unwrap()), "22 * (((44 + 66)))");
    assert_eq!(&format!("{}", pars.parse(&mut diags, "2147483648").unwrap()), "<error-expr>");
}

macro_rules! parse_string_check {
    ($Ast: ident, $input: literal, $expect: literal) => {
        {
            let parser = calc::$Ast::new();
            let mut diags = Vec::new();
            assert_eq!(&format!("{}", parser.parse(&mut diags, $input).unwrap()), $expect);
        }
    };
}

#[test]
fn local_var_declare() {
    parse_string_check!(SmtParser, "local k, b = 1, 2, 3", "local k, b = 1, 2, 3");
    parse_string_check!(ChunkParser, "local a = 1
    local b = a + 3
    local c = a * 2", "local a = 1;\nlocal b = a + 3;\nlocal c = a * 2;");
}

pub(crate) fn parse_string<'a>(input: &'a str) -> Chunk {
    let parser = calc::ChunkParser::new();
    let mut diags = Vec::new();
    let chunk = parser.parse(&mut diags, input).unwrap();
    chunk
}