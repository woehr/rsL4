#![crate_name = "rsl4-init"]
#![crate_type = "bin"]
#![no_std]
#![feature(asm, core, lang_items, no_std, start)]

#[macro_use]
extern crate core;
extern crate rsl4;

#[start]
pub fn rsl4_init(_: isize, _: * const * const u8) -> isize {
  loop {}
}

// stack_exhausted won't ever be called since we implement our own __morestack. Even so, the
// compiler still expects to find the function.
#[lang = "stack_exhausted"] extern fn stack_exhausted() {}

#[lang = "eh_personality"]  extern fn eh_personality() {}

// This function will only be called if a call to panic! is made.
#[lang = "panic_fmt"]              fn panic_fmt() -> ! {
  loop {}
}

// These functions are expected by something for stack unwinding.
#[no_mangle]
pub extern "C" fn __aeabi_unwind_cpp_pr0() {}
#[no_mangle]
pub extern "C" fn __aeabi_unwind_cpp_pr1() {}

