#![crate_name = "compiler-rt"]
#![crate_type = "staticlib"]
#![feature(lang_items)]
#![no_std]

#[lang="sized"]
trait Sized {}

pub extern fn dummy() {}

