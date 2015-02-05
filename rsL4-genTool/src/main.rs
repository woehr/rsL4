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

const INTRO : &'static str = "#![crate_type=\"lib\"]\n\
                              #![crate_name=\"rsl4\"]\n\
                              #![no_std]\n\
                              mod rsl4 {\n\
                              mod generated {\n\
                             ";
const OUTRO : &'static str = "}\n\
                              }\n\
                             ";

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
        h_out.write_all("\n".as_bytes()).unwrap();
    };

//    write_or_err(INTRO);

    for line in gen_syscalls(h_syscall).iter() {
        write_or_err(&line);
    }

    for line in gen_invocs(h_invoc, h_invoc_arch).iter() {
        write_or_err(&line);
    }

//    write_or_err(OUTRO);
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

    let mut ret_val = vec!["pub enum Syscall {".to_string()];
    for (i, ref sc) in syscalls.iter().enumerate() {
        ret_val.push(format!("  {} = {},", sc, -(i as isize + 1)));
    }
    ret_val.push("}".to_string());
    ret_val
}

fn gen_invocs(h_invoc: File, h_invoc_arch: File) -> Vec<String> {
    let invoc_reader = BufferedReader::new(h_invoc);
    let mut parser = EventReader::new(invoc_reader);

    let mut invocs = vec!["InvalidInvocation".to_string()];

    for e in parser.events() {
        match e {
            XmlEvent::StartElement { name, attributes, namespace: _ } => {
                if name.local_name == "method" {
                    invocs.push(attributes.iter().find(| &attr |{
                        attr.name.local_name == "id"
                    }).unwrap().value.clone());
                }
            },
            XmlEvent::Error(e) => panic!("Xml parsing error: {}", e),
            _ => {}
        }
    }

    let invoc_arch_reader = BufferedReader::new(h_invoc_arch);
    parser = EventReader::new(invoc_arch_reader);

    for e in parser.events() {
        match e {
            XmlEvent::StartElement { name, attributes, namespace: _ } => {
                if name.local_name == "method" {
                    invocs.push(attributes.iter().find(| &attr |{
                        attr.name.local_name == "id"
                    }).unwrap().value.clone());
                }
            },
            XmlEvent::Error(e) => panic!("Xml parsing error: {}", e),
            _ => {}
        }
    }

    let mut ret_val = vec!["pub enum InvocationLabel {".to_string()];
    for ref invoc_label in invocs.iter() {
        ret_val.push(format!("  {},", invoc_label));
    }
    ret_val.push("}".to_string());
    ret_val
}

