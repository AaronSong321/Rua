mod ast;
mod luac_api;
mod err;
pub mod value;
mod bc;
mod vm;

mod lib_core;

fn main() {
    println!("Hello, world!");
}

#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub calc);