use crate::vm::*;
use std::collections::hash_map::DefaultHasher;
use std::{fmt::*, collections::HashMap, rc::Rc, cell::RefCell};
use std::hash::{Hash, Hasher};

pub type LuaInt = i32;

// the type that represents the type and value of a lua value, equivalent to `TValue'
#[derive(Clone)]
pub enum Value {
    Nil,
    Int(LuaInt),
    String(Rc<String>),
    LString(&'static str),
    TableRef(Rc<RefCell<Table>>),
    RFun(LuaRFun),
}

#[derive(Clone, Debug)]
pub struct Table {
    pub arr: Vec<Value>,
    pub map: HashMap<Value, Value>,
}

#[derive(Copy, Clone, Debug)]
// lua function implemented in lua
pub struct LFun {}

// lua function signature implemented in rust
// this type is never known by lua
pub type LuaRFun = fn (&mut LuaState) -> isize;

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Int(i) => write!(f, "{}", i),
            Value::String(s) => {
                write!(f, "\"{}\"", *s)
            }
            Value::LString(s) => write!(f, "\"{}\"", *s),
            Value::RFun(f1) => write!(f, "rust function {:p}", f1),
            Value::TableRef(t) => write!(f, "table {:p}", t),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Int(i) => write!(f, "{i}"),
            Value::String(s) => write!(f, "{s}"),
            Value::LString(s) => write!(f, "{s}"),
            Value::RFun(f1) => write!(f, "rust function {:p}", f1),
            Value::TableRef(t) => write!(f, "table {:p}", t),
        }
    }
}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Nil => (),
            Value::Int(v) => v.hash(state),
            Value::String(s) => s.hash(state),
            Value::LString(s) => s.hash(state),
            Value::TableRef(t) => Rc::as_ptr(t).hash(state),
            Value::RFun(f) => (*f as *const usize).hash(state),
        }
    }
}

fn dh<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Int(s1), Value::Int(s2)) => *s1 == *s2,
            (Value::String(s1), Value::LString(s2)) => dh(s1) == dh(s2),
            (Value::String(s1), Value::String(s2)) => dh(s1) == dh(s2),
            (Value::LString(s1), Value::LString(s2)) => dh(s1) == dh(s2),
            (Value::LString(s1), Value::String(s2)) => dh(s1) == dh(s2),
            (Value::RFun(f1), Value::RFun(f2)) => *f1 as *const u8 == *f2 as *const u8,
            (Value::TableRef(r1), Value::TableRef(r2)) => Rc::as_ptr(r1) == Rc::as_ptr(r2),
            _ => false
        }
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl Eq for Value {
}

impl Table {
    pub(crate) fn get(&self, key: &Value) -> Value {
        return self.map[key].clone()
    }

    pub(crate) fn new() -> Table {
        Table {
            arr: Vec::new(),
            map: HashMap::new()
        }
    }
}