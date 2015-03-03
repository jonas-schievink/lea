//! The Lea compiler command line frontend

#![feature(core, old_io, old_path)]

extern crate lea;

use std::env;
use std::fmt;
use std::old_io::{stdio, File};

use lea::compiler::*;
use lea::program::Program;

macro_rules! printerr {
    ($($arg:tt)*) => {
        print_err_fmt(format_args!($($arg)*));
    }
}

#[inline]
#[allow(unused_must_use)]
fn print_err_fmt(fmt: fmt::Arguments) {
    writeln!(&mut stdio::stderr_raw(), "{}", fmt);
}

fn print_usage() {
    println!("Usage: leac [file]");
    println!("");
    println!("Parses and checks the given file. If an error occurs, outputs it.");
    println!("When the compiler is done, this will actually compile the source code.");
}

fn compile(code: &str, filename: &str) {
    let mut p = Program::new();
    match compile_str(&mut p, code, filename) {
        Err(e) => match e {
            ErrParse(err) => {
                printerr!("parse error: {}", err.format(code.as_slice(), filename));
                return;
            },
            ErrCheck(errs) => {
                let mut errstr = String::new();
                let mut i = 1;
                for err in &errs {
                    errstr.push_str("error: ");
                    errstr.push_str(err.format(code.as_slice(), filename).as_slice());
                    if i < errs.len() - 1 { errstr.push_str("\n"); }
                    i += 1;
                }
                printerr!("{}", errstr);
                return;
            },
        },
        Ok(_output) => {
            // TODO
            return;
        }
    }
}

pub fn main() {
    let mut iter = env::args();
    let mut inputarg: Option<String> = None;
    let mut lastarg: Option<String> = None;

    iter.next();    // Consume first argument (exec name)
    for arg in iter {
        if let Some(..) = inputarg {
            printerr!("Invalid argument: {}", lastarg.unwrap());
            print_usage();
            return;
        }

        match arg.as_slice() {
            "--help" | "-h" => {
                print_usage();
                return;
            },
            _ => {
                // Unknown arg, must be input file
                inputarg = Some(arg.clone());
            },
        }

        lastarg = Some(arg);
    }

    if let None = inputarg {
        inputarg = Some("-".to_string());   // read stdin by default
    }

    let f = inputarg.unwrap();
    if f.as_slice() == "-" {
        compile(stdio::stdin_raw().read_to_string().unwrap().as_slice(), "<stdin>");
    } else {
        let code = match File::open(&Path::new(f.clone())).read_to_string() {
            Ok(c) => c,
            Err(err) => {
                printerr!("error reading file {}: {}", f, err);
                return;
            }
        };

        compile(code.as_slice(), f.as_slice());
    }
}
