#![crate_name = "rsl4-init"]
#![crate_type = "bin"]
#![no_std]
#![feature(asm, core, lang_items, no_std, start)]

#[macro_use]
extern crate core;
extern crate rsl4;

use core::prelude::*;
use core::fmt;
use rsl4::syscalls::{debugPutChar};
use rsl4::helpers::{debugPutStr};

struct DebugWriter;
impl fmt::Write for DebugWriter {
  fn write_str(&mut self, s: &str) -> fmt::Result {
      debugPutStr(s);
      Ok(())
  }
}

#[start]
pub fn rsl4_init(_: isize, _: * const * const u8) -> isize {
  let mut print_me: u8 = 0x30;
  loop {
    debugPutChar(print_me);
    print_me = print_me + 1;
    if print_me > 0x39 {
      panic!();
    }
  }
}

// stack_exhausted won't ever be called since we implement our own __morestack. Even so, the
// compiler still expects to find the function.
#[lang = "stack_exhausted"]
extern fn stack_exhausted() {}

#[lang = "eh_personality"]
extern fn eh_personality() {}

// This function will only be called if a call to panic! is made.
#[lang = "panic_fmt"]
pub extern fn rust_begin_unwind(
    msg: fmt::Arguments,
    file: &'static str,
    _line: usize)
    -> !
{
  let mut debugWriter = DebugWriter;
  let _ = fmt::write(&mut debugWriter, msg);
  debugPutStr(file);

  loop {}
}

// These functions are expected by something for stack unwinding.
#[no_mangle]
pub extern "C" fn __aeabi_unwind_cpp_pr0() {}
#[no_mangle]
pub extern "C" fn __aeabi_unwind_cpp_pr1() {}

