use crate::{value::LuaInt, ast::*};


pub(crate) struct LocalVar<'a> {
    pub name: &'a str,
    pub lid: usize,
}

pub(crate) struct CompiledBC<'a> {
    pub(crate) kints: Vec<LuaInt>,
    pub(crate) kstrs: Vec<&'a str>,
    pub(crate) code: Vec<BC>,
    pub(crate) locals: Vec<LocalVar<'a>>,
}

enum ExprResultId {
    None,
    One(isize),
    Many(Vec<isize>)
}

impl<'a> CompiledBC<'a> {
    fn name_lookup(&self, name: &'a str) -> Option<usize> {
        for i in (0..self.locals.len()).rev() {
            if self.locals[i].name == name {
                return Some(self.locals[i].lid);
            }
        }
        return None;
    }

    fn get_int_constable_id(&mut self, v: LuaInt) -> usize {
        if let Some(id) = self.kints.iter().position(|&t| t == v) {
            id
        } else {
            self.kints.push(v);
            self.kints.len() - 1
        }
    }

    fn get_str_constable_id(&mut self, v: &'a str) -> usize {
        if let Some(id) = self.kstrs.iter().position(|&t| t == v) {
            id
        } else {
            self.kstrs.push(v);
            self.kstrs.len() - 1
        }
    }

    // returns the stack index to the value represented by id \p name
    fn id_expr_to_index(&mut self, name: &'a str) -> isize {
        match self.name_lookup(name) {
            Some(id) => id as isize,
            None => {
                let kid = self.get_str_constable_id(name);
                self.code.push(BC::LdG(kid));
                -1
            }
        }
    }

    fn op_to_stack_top(&mut self, le: &'a Expr<'a>, op: Opcode, re: &'a Expr<'a>) {
        let ExprResultId::One(lhs_id) = self.expr_to_index(le) else { panic!() };
        let ExprResultId::One(rhs_id) = self.expr_to_index(re) else { panic!() };
        match op {
            Opcode::Add => self.code.push(BC::IAdd(lhs_id, rhs_id)),
            Opcode::Mul => self.code.push(BC::IMul(lhs_id, rhs_id)),
        }
    }

    // compile an expression, and returns the indices of the result of this expression (if any)
    fn expr_to_index(&mut self, expr: &'a Expr<'a>) -> ExprResultId {
        match expr {
            Expr::Id(v) => ExprResultId::One(self.id_expr_to_index(v)),
            _ => {
                self.expr_to_stack_top(expr);
                ExprResultId::One(-1)
            }
        }
    }
    fn id_expr_to_stack_top(&mut self, name: &'a str) {
        match self.name_lookup(name) {
            Some(lid) => self.code.push(BC::LdLoc(lid)),
            None => {
                let kid = self.get_str_constable_id(name);
                self.code.push(BC::LdG(kid));
            }
        }
    }


    // compile an expression, and push the results of the expression on the stack top (if any)
    fn expr_to_stack_top(&mut self, expr: &'a Expr<'a>) {
        use ExprResultId::*;
        match expr {
            Expr::Num(v) => {
                let kid = self.get_int_constable_id(*v);
                self.code.push(BC::KInt(kid));
            }
            Expr::Op(le, op, re) => {
                let One(lhs_id) = self.expr_to_index(le) else { panic!() };
                let One(rhs_id) = self.expr_to_index(re) else { panic!() };
                match op {
                    Opcode::Add => self.code.push(BC::IAdd(lhs_id, rhs_id)),
                    Opcode::Mul => self.code.push(BC::IMul(lhs_id, rhs_id)),
                }
            }
            Expr::Paren(v) => self.expr_to_stack_top(v),
            Expr::Id(v) => self.id_expr_to_stack_top(v),
            Expr::Call(f, args) => {
                self.expr_to_stack_top(f);
                for i in 0..args.len() {
                    if i != args.len()-1 {
                        self.expr_to_stack_top(&args[i]);
                    } else {
                        self.expr_to_stack_top(&args[i]);
                    }
                }
                self.code.push(BC::CallN(args.len() as isize, 1, false));
            }
            Expr::ErrorExpr => panic!(),
        }
    }

    fn introduce_local(&mut self, name: &'a str) -> usize {
        self.locals.push(LocalVar { name, lid: self.locals.len() + 1 });
        return self.locals.len();
    }

    fn compile_smt(&mut self, smt: &'a Smt<'a>) {
        match smt {
            Smt::LocalVarDecl(lhs, rhs) => {
                assert_eq!(lhs.len(), 1);
                let r1 = rhs.as_ref().unwrap();
                assert_eq!(r1.len(), 1);
                let value = &r1[0];
                self.expr_to_index(value);
                let id = self.introduce_local(lhs[0].id);
                self.code.push(BC::StLoc(id));
            }
        }
    }

    fn compile_chunk(&mut self, chunk: &'a Chunk<'a>) -> () {
        for smt in chunk.v.iter() {
            self.compile_smt(smt);
        }
    }
}

pub(crate) fn ast2bc<'a>(ast: &'a Chunk<'a>) -> CompiledBC<'a> {
    let mut compiler = CompiledBC {
        kints: Vec::new(),
        kstrs: Vec::new(),
        code: Vec::new(),
        locals: Vec::new(),
    };
    compiler.compile_chunk(ast);
    compiler
}

#[test]
fn test() {
    let chunk = parse_string("local a = 1
    local c = a * 15
    local d = a + c
    local b = print(d)");
    let bc = ast2bc(&chunk);
    assert_eq!(format!("{:?}", bc.code), "[KInt(0), StLoc(1), KInt(1), IMul(1, -1), StLoc(2), IAdd(1, 2), StLoc(3), LdG(0), LdLoc(3), CallN(1, 1, false), StLoc(4)]");
    assert_eq!(format!("{:?}", bc.kints), "[1, 15]");
}

pub(crate) const RETNUM_VARARG: isize = -1;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BC {
    KInt(usize),
    KString(usize),
    IAdd(isize, isize),
    IMul(isize, isize),
    CallN(isize, isize, bool), // arg_num, required_ret_num, use_vararg
    LdLoc(usize),
    // load a global value by a string key, which is guaranteed to be on the stack top
    LdG(usize), // load global variable by key represented by the argument, which is an index into the constant
    // string table
    StLoc(usize),
}