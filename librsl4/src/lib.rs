#![crate_type="lib"]
#![crate_name="rsl4"]
#![no_std]
#![feature(asm, core, no_std)]

#[macro_use]
extern crate core;

// This module corresponds to sel4/sel4.h

pub mod boot_info;
pub mod syscalls;
pub mod types;

