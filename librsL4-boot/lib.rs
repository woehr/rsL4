#![crate_name = "rsL4-boot"]
#![crate_type = "rlib"]
#![no_std]
#![feature(lang_items)]

extern crate core;

use core::intrinsics::{volatile_load, volatile_store};
use core::str::StrPrelude;

#[allow(dead_code)]
extern {
  fn increase_stack();
}

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

fn recurse() {
  put_str("a");
  recurse();
}

#[no_mangle]
#[allow(dead_code)]
pub extern fn main() {
  recurse();
}

// stack_exhausted won't ever be called since we implement our own __morestack. Even so, the
// compiler still expects to find the function.
#[lang = "stack_exhausted"] extern fn stack_exhausted() {}

#[lang = "eh_personality"]  extern fn eh_personality() {}

// This function will only be called if a call to panic! is made.
#[lang = "panic_fmt"]              fn panic_fmt() -> ! {
  put_str("panic_fmt called");
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
  put_str("Stack overflow");
  loop {}
}

// These functions are expected by something for stack unwinding.
#[no_mangle]
extern fn __aeabi_unwind_cpp_pr0() {}
#[no_mangle]
extern fn __aeabi_unwind_cpp_pr1() {}

