use std::cell::{RefCell, Ref};
use std::cmp::{min, max};
use std::collections::{LinkedList, HashMap};
use std::io::{BufWriter, Write};
use std::ops::Deref;
use std::ptr;
use std::rc::{Weak, Rc};

use crate::ast::parse_string;
use crate::{bc::*};
use crate::calc::{self};
use crate::lib_core::loadlib_core;
use crate::value::*;


pub struct GlobalState {
    gclist: LinkedList<Weak<RefCell<Table>>>,
    gtable: Rc<RefCell<Table>>,
    
    threads: LinkedList<LuaState>,
    cur_th: *mut LuaState,
}

pub struct LuaState {
    stack: Vec<Value>,

    // call info stack
    cistack: LinkedList<CallInfo>,
    global_st: *mut GlobalState,
}

pub fn new_lua() -> GlobalState {
    let mut gs = GlobalState {
        gclist: LinkedList::new(),
        gtable: Rc::new(RefCell::new(Table::new())),

        threads: LinkedList::new(),
        cur_th: std::ptr::null_mut(),
    };
    gs.gclist.push_back(Rc::downgrade(&gs.gtable));
    let mut main_th = LuaState {
        stack: vec![Value::Nil; STACK_SIZE],
        cistack: LinkedList::new(),
        global_st: &mut gs,
    };
    main_th.cistack.push_back(CallInfo {
        ip: 0,
        func: 0,
        sp: 1,
        sb: 1,
        previous: ptr::null(),
        next: ptr::null(),
        vararg_num: None,
        nres: None,
    });

    gs.threads.push_back(main_th);
    gs.cur_th = gs.threads.back_mut().unwrap();
    gs
}

impl GlobalState {
    pub(crate) fn load_into_global_table(&mut self, key: &'static str, value: LuaRFun) {
        self.gtable.borrow_mut().map.insert(Value::LString(key), Value::RFun(value));
    }

    fn get_global(&self, key: &str) -> Value {
        let keyv = Value::String(Rc::new(key.to_string()));
        return self.gtable.borrow().get(&keyv);
    }

    pub(crate) fn exec_main(&mut self, bc: &CompiledBC) -> i32 {
        unsafe {
            (*self.cur_th).exec(bc);
            0
        }
    }
}

struct CallInfo {
    ip: usize, // instruction pointer
    func: isize, // the function that is being executed
    sb: isize, // stack base, pointing to first argument
    sp: isize, // stack pointer, pointing to the next temporary to be created

    previous: *const CallInfo,
    next: *const CallInfo,
    vararg_num: Option<isize>,
    nres: Option<isize>, // number of result of last instruction, used when last instruction returns vararg
}

impl CallInfo {
    fn check_last_inst_vararg_num(&self) -> isize {
        let res = match self.vararg_num {
            Some(v) => v,
            None => 0
        };
        res
    }

    fn set_result_num(&mut self, nres: isize) {
        self.nres = Some(nres);
    }
}

enum IntOp {
    Add,
    Mul,
}

static STACK_SIZE: usize = 30;

// basic stack manipulating functions
impl LuaState {
    fn topci_mut(&mut self) -> *mut CallInfo {
        self.cistack.back_mut().unwrap()
    }
    fn topci(&self) -> *const CallInfo {
        self.cistack.back().unwrap()
    }

    fn exec(&mut self, bc: &CompiledBC) {
        let ci = self.topci_mut();
        unsafe {
            (*ci).ip = 0;
            (*ci).sb = 1;
            (*ci).sp = (*ci).sb + bc.locals.len() as isize;
        }
        self.exec_next_step(bc);
    }

    fn exec_next_step(&mut self, bc: &CompiledBC) {
        let ci = self.topci_mut();
        unsafe {
            while (*ci).ip != bc.code.len() {
                self.exec_bc(&bc.code[(*ci).ip], bc);
                (*ci).ip += 1;
            }
        }
    }

    fn exec_bc(&mut self, inst: &BC, bc: &CompiledBC) {
        match inst {
            BC::IAdd(l, r) => self.bc_iop(*l, *r, IntOp::Add),
            BC::IMul(l, r) => self.bc_iop(*l, *r, IntOp::Mul),
            BC::KInt(kid) => self.lua_pushinteger(bc.kints[*kid]),
            BC::KString(sid) => self.lua_pushstring(bc.kstrs[*sid].to_string()),
            BC::LdG(kid) => self.bc_load_global(bc.kstrs[*kid]),
            BC::LdLoc(id) => self.bc_load_local(*id as isize),
            BC::StLoc(id) => self.bc_stloc(*id as isize),
            BC::CallN(arg_num, out_num, vararg) => self.bc_calln(*arg_num as isize, *out_num, *vararg),
        }
    }

    // set the number of elements in the stack
    pub fn lua_settop(&mut self, index: isize) {
        let old_sp = self.lua_gettop();
        let new_sp = if index >= 0 { index }
            else { old_sp + index + 1 };
        // remove values at higher index than new sp
        // todo: these values need not set to nil explicitly had the garbage collection is built
        for i in old_sp..new_sp {
            self.stack[i as usize] = Value::Nil;
        }
        // set new values to nil
        // todo: also these values need not set to nil explicitly had the values are always set to nil
        // upon the creation of the LuaState also upon the removal from stack
        // but now, set to nil explicitly for safety issue
        for i in new_sp..old_sp {
            self.stack[i as usize] = Value::Nil;
        }
        unsafe {
            let ci = self.topci_mut();
            (*ci).sp = (*ci).sb + new_sp;
        }
    }

    pub fn lua_pushvalue(&mut self, index: isize) {
        self.inc_sp(1);
        self.copy(-1, index);
    }

    pub fn lua_checkstack(&mut self, sz: isize) {
        assert!(self.get_sp() + sz < STACK_SIZE as isize);
    }

    fn get_sp(&self) -> isize {
        let a = self.cistack.back().unwrap().sp;
        a
    }

    fn inc_sp(&mut self, index: isize) {
        self.cistack.back_mut().unwrap().sp += index;
    }

    pub fn lua_remove(&mut self, index: isize) {
        let stackid = self.get_stack_index(index);
        let old_sp = self.get_sp();
        assert!(old_sp > stackid as isize);
        for i in (stackid+1)..(old_sp as usize) {
            self.stack[i] = self.stack[i + 1].clone();
        }
        self.inc_sp(-1);
    }

    pub fn lua_replace(&mut self, index: isize) {
        let v = self.get_temp_ref(-1);
        self.set_temp(index, v.clone());
        self.inc_sp(-1);
    }

    pub fn lua_insert(&mut self, index: isize) {
        todo!()
    }

    pub fn lua_pop(&mut self, sz: isize) {
        self.lua_settop(-sz - 1);
    }

    // create a LuaValue containing a reference to a static string on the stack top
    pub fn lua_pushlstring(&mut self, s: &'static str) {
        self.inc_sp(1);
        self.set_temp(-1, Value::LString(s));
    }

    // create a LuaValue that owns a string created from a reference to a temp string and push it on the stack top
    pub fn lua_pushstr(&mut self, s: &'_ str) {
        self.inc_sp(1);
        self.set_temp(-1, Value::String(Rc::new(s.to_string())));
    }

    pub fn lua_pushstring(&mut self, s: String) {
        self.inc_sp(1);
        self.set_temp(-1, Value::String(Rc::new(s)));
    }

    // create a LuaValue that contains an integer with the given value
    pub fn lua_pushinteger(&mut self, v: LuaInt) {
        self.inc_sp(1);
        self.set_temp(-1, Value::Int(v));
    }

    fn get_stack_index(&self, id: isize) -> usize {
        let ci = self.topci();
        unsafe {
            if id > 0 {
                let realid = (*ci).sb + id - 1;
                assert!(realid < (*ci).sp);
                realid as usize
            } else if id < 0 {
                let realid = (*ci).sp + id;
                assert!(realid >= (*ci).sb);
                realid as usize
            } else {
                panic!("zero index is never valid in lua.");
            }
        }
    }

    pub fn get_temp(&self, id: isize) -> Value {
        self.stack[self.get_stack_index(id)].clone()
    }

    pub fn get_temp_ref(&self, id: isize) -> &Value {
        &self.stack[self.get_stack_index(id)]
    }

    pub fn set_temp(&mut self, id: isize, value: Value) {
        let stackid = self.get_stack_index(id);
        self.stack[stackid] = value
    }

    fn get_int_checked(&self, v: &Value) -> LuaInt {
        match v {
            Value::Int(v1) => *v1,
            _ => panic!(),
        }
    }

    fn bc_load_global(&mut self, key: &'_ str) {
        self.inc_sp(1);
        self.set_temp(-1, unsafe { (*self.global_st).get_global(key) });
    }

    fn get_str_checked(v: &Value) -> Rc<String> {
        match v {
            Value::String(v) => v.clone(),
            _ => panic!()
        }
    }

    pub fn getstring(&self, id: isize) -> Rc<String> {
        Self::get_str_checked(&self.get_temp(id))
    }

    fn get_rfun_checked(v: &Value) -> LuaRFun {
        match v {
            Value::RFun(v1) => *v1,
            _ => panic!()
        }
    }

    fn bc_load_local(&mut self, id: isize) {
        self.inc_sp(1);
        self.set_temp(-1, self.get_temp(id));
    }

    // a little hack here: id below 0 are interpreted as temporary id while id above 0 local id.
    fn is_temp_id(id: isize) -> bool {
        id < 0
    }

    fn bc_iop(&mut self, lhs_id: isize, rhs_id: isize, op: IntOp) {
        let lhs = self.get_int_checked(&self.get_temp(lhs_id));
        let rhs = self.get_int_checked(&self.get_temp(rhs_id));
        let res = match op {
            IntOp::Add => lhs + rhs,
            IntOp::Mul => lhs * rhs,
        };
        let temp_num = if Self::is_temp_id(lhs_id) { 1 } else { 0 } + if Self::is_temp_id(rhs_id) { 1 } else { 0 };
        let res_id: isize;
        if temp_num == 0 {
            self.inc_sp(1);
            res_id = -1;
        } else if temp_num == 1 {
            res_id = -1;
        } else {
            res_id = -2;
        }
        self.set_temp(res_id, Value::Int(res));
        if temp_num == 2 {
            self.lua_pop(1);
        }
    }

    fn bc_stloc(&mut self, id: isize) {
        self.set_temp(id, self.get_temp(-1));
        self.lua_pop(1);
    }

    // returns the absolute index of \p id
    pub fn absindex(&mut self, id: isize) -> isize {
        if id == 0 {
            panic!("0 as stack id is never valid.");
        } else if id < 0 {
            self.get_sp() + id
        } else {
            id
        }
    }

    fn bc_calln(&mut self, arg_num: isize, adjusted_out_num: isize, use_vararg: bool) {
        assert!(arg_num >= 0);
        let fun_value = self.get_temp(-arg_num - 1);
        match fun_value {
            Value::RFun(rfun) => {
                // push new call info
                let call_time_sp = self.get_sp();
                let mut ci: *mut CallInfo = self.topci_mut();
                let actual_arg_num = arg_num + unsafe { (*ci).check_last_inst_vararg_num() }; // arg num + vararg num
                let new_ci = CallInfo {
                    ip: 0,
                    func: self.absindex(-actual_arg_num - 1),
                    sb: self.absindex(-actual_arg_num),
                    sp: unsafe { (*ci).sp },
                    previous: ci,
                    next: ptr::null(),
                    nres: None,
                    vararg_num: None,
                };
                self.cistack.push_back(new_ci);
                unsafe {
                    (*ci).next = self.topci();
                }

                // call the rust function
                let ret_num = rfun(self);

                // adjust return value
                let rfun_call_input_num = actual_arg_num + 1; // all inputs, including args, varargs, and the function to be called
                assert_eq!(self.lua_gettop(), actual_arg_num + ret_num);

                if adjusted_out_num == RETNUM_VARARG {
                    unsafe {
                        (*ci).set_result_num(ret_num);
                    }
                } else {
                    // replace input values with returned values
                    for ret_value_id in 0..min(ret_num, adjusted_out_num) {
                        let func_pos = unsafe { (*self.topci()).func };
                        self.copy_unchecked((func_pos + ret_value_id) as usize, (self.get_sp() + ret_value_id) as usize);
                    }
                    // push nil value if adjusted output count is more than the number of return values
                    for _ in ret_num..adjusted_out_num {
                        let func = unsafe { (*self.topci()).func };
                        self.stack[(func + ret_num) as usize] = Value::Nil;
                    }
                }

                // adjust stack
                self.lua_settop(adjusted_out_num);
                self.inc_sp(-1); // pop the function position
                let callee_sp = self.get_sp();
                assert!(call_time_sp - rfun_call_input_num + adjusted_out_num == callee_sp);

                // pop call info
                self.cistack.pop_back();
                unsafe { (*self.topci_mut()).sp = callee_sp; }
            },
            _ => panic!("Non callable value {:?}", fun_value)
        }
    }

    // copy a LuaValue located at \p src to \p dst
    pub fn copy(&mut self, dst: isize, src: isize) {
        self.set_temp(dst, self.get_temp(src));
    }

    // copy a LuaValue at stack index \p src to index \p dst. The arguments are direct index to the stack and undergo
    // no safety checks
    fn copy_unchecked(&mut self, dst: usize, src: usize) {
        self.stack[dst] = self.stack[src].clone();
    }

    pub fn lua_pushnil(&mut self) {
        self.inc_sp(1);
        self.set_temp(-1, Value::Nil);
    }

    // get difference of stack pointer to stack base, a.k.a the number of args, varargs, locals, and temporaries of this function. When no values are pushed onto the stack, this function
    // returns the number of arguments in a lua Rust function.
    pub fn lua_gettop(&self) -> isize {
        let ci = self.topci();
        unsafe {
            (*ci).sp - (*ci).sb
        }
    }

    // todo: untested
    pub fn lua_rawequal(&self, id1: isize, id2: isize) -> isize {
        let res = match (self.get_temp_ref(id1), self.get_temp_ref(id2)) {
            (Value::Nil, Value::Nil) => true,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::String(a), Value::String(b)) => *a == *b,
            (Value::LString(a), Value::String(b)) => *a == &b[..],
            (Value::String(a), Value::LString(b)) => &a[..] == *b,
            (Value::LString(a), Value::LString(b)) => &a[..] == &b[..],
            (Value::RFun(a), Value::RFun(b)) => *a as *const u8 == *b as *const u8,
            (Value::TableRef(a), Value::TableRef(b)) => a.as_ptr() == b.as_ptr(),
            _ => false,
        };
        0
    }
}

#[test]
fn test_vararg_print() {
    let chunk = parse_string("local a = 1
    local c = a * 3
    local d = a + c
    local b = print(d)
    local e = c + d
    local f = print(a,b,c,d,e)
    "); // 4 printed
    let bc = ast2bc(&chunk);
    assert_eq!(format!("{:?}", bc.code), "[KInt(0), StLoc(1), KInt(1), IMul(1, -1), StLoc(2), IAdd(1, 2), StLoc(3), LdG(0), LdLoc(3), CallN(1, 1, false), StLoc(4), IAdd(2, 3), StLoc(5), LdG(0), LdLoc(1), LdLoc(4), LdLoc(2), LdLoc(3), LdLoc(5), CallN(5, 1, false), StLoc(6)]");
    let mut lua = new_lua();
    loadlib_core(&mut lua);
    lua.exec_main(&bc);
}

#[test]
fn test_vararg() {
    fn luaL_rev(lua: &mut LuaState) -> isize {
        let argn = lua.lua_gettop();
        for i in 1..=argn {
            lua.lua_pushvalue(i);
        }
        argn
    }

    let chunk = parse_string("local a = 1
    local b = a + a
    local c = 15
    local d = rev(a,b,c)
    local e = rev(a,b,c,d)
    ");
    let bc = ast2bc(&chunk);
    assert_eq!(format!("{:?}", bc.code), "[KInt(0), StLoc(1), IAdd(1, 1), StLoc(2), KInt(1), StLoc(3), LdG(0), LdLoc(1), LdLoc(2), LdLoc(3), CallN(3, 1, false), StLoc(4), LdG(0), LdLoc(1), LdLoc(2), LdLoc(3), LdLoc(4), CallN(4, 1, false), StLoc(5)]");
    let mut lua = new_lua();
    loadlib_core(&mut lua);
    lua.load_into_global_table("rev", luaL_rev);
    lua.exec_main(&bc);
}