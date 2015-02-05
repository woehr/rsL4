#![crate_type="lib"]
#![crate_name="rsl4"]
#![no_std]
#![feature(core)]

#[macro_use]
extern crate core;

// This module corresponds to sel4/sel4.h

pub mod boot_info;
pub mod types;

