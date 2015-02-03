//! This program reads seL4 interface files and generates a library. This is
//! accomplished using templates. In the future something more sophisticated
//! may be interesting.

#![feature(collections, io, os, path)]

extern crate xml;

use std::old_io::{File, BufferedReader, stderr};
use std::os::{args};

use xml::reader::EventReader;
use xml::reader::events::*;

const NUM_INPUTS  : usize = 3;
const NUM_OUTPUTS : usize = 1;

const INTRO : &'static str = "mod rsL4 {\nmod generated {\n";
const OUTRO : &'static str = "}\n}\n";

const MOD_SYS        : &'static str = "syscall";
const MOD_INVOC      : &'static str = "invocation";
const MOD_INVOC_ARCH : &'static str = "invocation_arch";

fn main() {
    let argv = args();

    // TODO: We just generated all the syscalls but depending on how the kernel
    // was built some may not be included which can affect the offset of later
    // constants.
    if argv.len() != NUM_INPUTS + NUM_OUTPUTS + 1 { // +1 for executable name
        display_help(&argv[0]);
        return;
    }

    // open seL4 xml interface files
    let h_syscall    = File::open(&Path::new(&argv[1])).unwrap();
    let h_invoc      = File::open(&Path::new(&argv[2])).unwrap();
    let h_invoc_arch = File::open(&Path::new(&argv[3])).unwrap();

    // output file
    let mut h_out    = File::create(&Path::new(&argv[4])).unwrap();
    let mut write_or_err = |&mut: s: &str| {
        h_out.write_all(s.as_bytes()).unwrap();
    };

    write_or_err(INTRO);

    write_or_err(&format!("mod {} {{\n", MOD_SYS));
    for line in gen_syscalls(h_syscall).iter() {
        write_or_err(&line);
    }
    write_or_err("}\n");

    write_or_err(OUTRO);
}

fn display_help(exe: &String) {
    let exe_path = Path::new(exe);
    let exe_name = exe_path.filename_str().unwrap();
    let mut err = stderr();
    write!(&mut err, "Usage: {} <syscall_spec> <invoc_spec> <invoc_arch_spec>\n", exe_name).unwrap();
}

fn gen_syscalls(h_in: File) -> Vec<String> {
    let reader = BufferedReader::new(h_in);
    let mut parser = EventReader::new(reader);
    let mut syscalls = vec![];

    for e in parser.events() {
        match e {
            XmlEvent::StartElement { name, attributes, namespace: _ } => {
                if name.local_name == "syscall" {
                    // search for an attribute that has a name of "name". The value is the name of
                    // the syscall
                    syscalls.push(attributes.iter().find(| &attr |{
                        attr.name.local_name == "name"
                    }).unwrap().value.clone());
                }
            },
            XmlEvent::Error(e) => panic!("Xml parsing error: {}", e),
            _ => {}
        }
    }

    let mut ret_val = vec!["struct Syscall(isize);".to_string()];
    for (i, ref sc) in syscalls.iter().enumerate() {
        ret_val.push(format!("const {} : Syscall = Syscall({});\n", sc, -(i as isize + 1)));
    }
    ret_val
}

