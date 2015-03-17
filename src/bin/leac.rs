//! The Lea compiler command line frontend

#![feature(core, lea)]

extern crate lea;

use std::env;
use std::fmt;
use std::default::Default;
use std::io;
use std::io::{Read, Write};
use std::fs::File;
use std::path::Path;

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
    writeln!(&mut io::stderr(), "{}", fmt);
}

fn print_usage() {
    println!("Usage: leac [file]");
    println!("");
    println!("Parses and checks the given file. If file is \"-\", reads stdin.");
    println!("When the compiler is done, this will actually compile the source code.");
}

fn compile(code: &str, filename: &str) {
    let mut p = Program::new();
    match compile_str(&mut p, code, filename, &CompileConfig::default()) {
        Err(e) => match e {
            ErrParse(err) => {
                printerr!("parse error: {}", err.format(code.as_slice(), filename));
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
            },
            ErrLint(warns) => {
                let mut first = true;
                for w in warns {
                    if first { first = false; } else { printerr!("\n"); }
                    printerr!("warning as error: {}", w.format(code.as_slice(), filename).as_slice());
                }
            },
            ErrEmit(err) => {
                match err.detail {
                    None => printerr!("emit error: {}", err.msg),
                    Some(detail) => printerr!("emit error: {} ({})", err.msg, detail),
                };
            }
        },
        Ok(output) => {
            let warns = output.get_warns();
            if warns.len() > 0 {
                println!("{} warnings:", warns.len());
                for w in warns {
                    println!("{}", w.format(code, filename));
                }
            }
        }
    }
}

fn compile_file(filename: &str) {
    match File::open(&Path::new(filename.clone())) {
        Ok(mut file) => {
            let mut code = String::new();
            match file.read_to_string(&mut code) {
                Ok(_) => {
                    compile(code.as_slice(), filename.as_slice());
                },
                Err(err) => {
                    printerr!("error reading file \"{}\": {}", filename, err);
                }
            }
        },
        Err(err) => {
            printerr!("error opening file \"{}\": {}", filename, err);
        }
    };
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
        printerr!("error: no input file");
        print_usage();
        return;
    }

    let f = inputarg.unwrap();
    if f.as_slice() == "-" {
        let mut code = String::new();
        io::stdin().read_to_string(&mut code).unwrap();

        println!("");   // print newline to seperate leac output from source input
        compile(code.as_slice(), "<stdin>");
    } else {
        compile_file(f.as_slice());
    }
}
