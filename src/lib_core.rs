use std::io::BufWriter;
use std::io::Read;
use std::io::stdout;

use crate::value::*;
use crate::vm::GlobalState;
use crate::vm::LuaState;


fn luaL_tostring(lua: &mut LuaState) -> isize {
    let v = lua.get_temp(1);
    lua.lua_checkstack(1);
    match v {
        Value::String(_) => {
            lua.copy(-1, 1);
        }
        Value::LString(_) => {
            lua.copy(-1, 1);
        }
        Value::Int(num) => {
            lua.lua_pushstring(num.to_string());
        }
        Value::Nil => lua.lua_pushstring(String::from("nil")),
        Value::RFun(rfn) => lua.lua_pushstring(format!("function {:p}", rfn as *const u8)),
        Value::TableRef(_) => todo!(),
    }
    return -1;
}

// the print function
fn core_print_value(lua: &LuaState, v: &Value, w: &mut dyn std::io::Write) {
    match v {
        Value::String(s) => write!(w, "{s}"),
        Value::Nil => write!(w, "nil"),
        Value::Int(i) => write!(w, "{i}"),
        Value::LString(s) => write!(w, "{s}"),
        Value::TableRef(t) => write!(w, "table {:p}", t.as_ptr()),
        Value::RFun(f) => write!(w, "function {:p}", *f as *const u8),
    }.unwrap();
}

fn core_print(lua: &LuaState, w: &mut dyn std::io::Write) {
    let n = lua.lua_gettop();
    for i in 1..=n {
        if i > 1 {
            write!(w, "\t").unwrap();
        }
        core_print_value(lua, lua.get_temp_ref(i), w);
    }
    writeln!(w, "").unwrap();
}

fn luaL_print(lua: &mut LuaState) -> isize {
    core_print(lua, &mut stdout());
    0
}

// this version of print is for testing, that is, 
// values are printed to a lua string and the result string is pushed onto thestack top
fn luaT_print(lua: &mut LuaState) -> isize {
    let mut writer = BufWriter::new(Vec::<u8>::new());
    core_print(lua, &mut writer);
    let mut write_result = String::new();
    writer.buffer().read_to_string(&mut write_result).unwrap();
    lua.lua_checkstack(1);
    lua.lua_pushstring(write_result);
    1
}

pub fn loadlib_core(g: &mut GlobalState) -> () {
    g.load_into_global_table("tostring", luaL_tostring);
    g.load_into_global_table("print", luaL_print);
}

pub fn loadlib_core_test(g: &mut GlobalState) -> () {
    g.load_into_global_table("print", luaT_print);
}