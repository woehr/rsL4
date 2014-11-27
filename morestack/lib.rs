#![crate_name = "morestack"]
#![crate_type = "staticlib"]
#![no_std]

#[no_mangle]
pub extern "C" fn __morestack() -> ! {
  loop {}
}

