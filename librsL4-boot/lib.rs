#![crate_name = "rsL4-boot"]
#![crate_type = "rlib"]
#![no_std]
#![feature(lang_items, phase)]

#[phase(plugin, link)]
extern crate core;

use core::prelude;
use core::fmt;
use core::intrinsics::{volatile_load, volatile_store};
use core::result::{Ok};

#[allow(dead_code)]
extern {
  fn increase_stack();
}

struct Uart {
  thr: * mut u8,
  sta: * const u8,
}

impl fmt::FormatWriter for Uart {
  fn write(&mut self, bytes: &[u8]) -> fmt::Result {
    for &b in bytes.iter() {
      unsafe {
        while volatile_load(self.sta) & 0x20 == 0 {}
        volatile_store(self.thr, b);
      }
    }
    Ok(())
  }
}

static uart0: Uart = Uart {thr: 0x44E09000 as * mut u8, sta: 0x44E09014 as * const u8};

/*
fn put_c(c: u8) {
  let thr: * mut u8 = 0x44E09000 as * mut u8;
  let sta: * const u8 = (0x44E09014) as * const u8;
  unsafe {
    while volatile_load(sta) & 0x20 == 0 {}
    volatile_store(thr, c);
  }
}

fn put_str(s: &str) {
  for b in s.bytes() {
    put_c(b);
  }
}
*/

fn recurse(call_count: u32) {
//  fmt::write(uart0, "{},", call_count);
  recurse(call_count + 1);
}



#[no_mangle]
#[allow(dead_code)]
pub extern fn main(argc: u32, argv: * const * const u8) {
  format_args!(|args| { write!(uart0, "{}", args) }, "There were {} args.\n", argc as u8);
//  write!(uart0, "There were {} args.\n", argc as u8);
  recurse(0);
}

// stack_exhausted won't ever be called since we implement our own __morestack. Even so, the
// compiler still expects to find the function.
#[lang = "stack_exhausted"] extern fn stack_exhausted() {}

#[lang = "eh_personality"]  extern fn eh_personality() {}

// This function will only be called if a call to panic! is made.
#[lang = "panic_fmt"]              fn panic_fmt() -> ! {
//  put_str("panic_fmt called");
  loop {}
}

// LLVM checks every function entry to make sure enough stack is allocated, if it isn't this
// function is called. This is normally used in a split stack situation to allocate more stack
// but Rust uses a call to this function to determine when the stack overflows (Rust uses a large
// stack instead of a split-stack). We also disable stack checking on __morestack so we don't try
// calling __morestack from running out of stack while executing __morestack.
#[no_mangle]
#[no_stack_check]
fn __morestack() {
  // Increase the stack limit so we can safely call into stack allocating functions. This does not
  // overflow the stack since space was left at the end of the stack for this purpose.
  unsafe {
    increase_stack();
  }
//  put_str("Stack overflow");
  loop {}
}

// These functions are expected by something for stack unwinding.
#[no_mangle]
extern fn __aeabi_unwind_cpp_pr0() {}
#[no_mangle]
extern fn __aeabi_unwind_cpp_pr1() {}

