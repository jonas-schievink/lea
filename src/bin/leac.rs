//! The Lea compiler command line frontend

#![feature(core, lea)]

extern crate lea;
extern crate term;

use lea::compiler::*;
use lea::compiler::span::*;

use std::env;
use std::fmt;
use std::default::Default;
use std::io::{self, Read, Write};
use std::fs::File;
use std::path::Path;


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

fn compile(code: &str, filename: &str) -> io::Result<()> {
    let mut err = io::stderr();
    let mut t = term::stderr();
    let mut fmt_target: FormatTarget<io::Stderr> = match t {
        Some(ref mut term) => FormatTarget::Term(&mut **term),
        None => FormatTarget::Io(&mut err),
    };

    match compile_str(code, filename, &CompileConfig::default()) {
        Err(e) => match e {
            ErrParse(err) => {
                try!(err.format(code.as_slice(), filename, &mut fmt_target));
            },
            ErrCheck(errs) => {
                let mut i = 1;
                for err in &errs {
                    try!(err.format(code.as_slice(), filename, &mut fmt_target));
                    if i < errs.len() - 1 { try!(write!(&mut fmt_target, "\n")); }
                    i += 1;
                }
            },
            ErrLint(warns) => {
                let mut first = true;
                for w in &warns {
                    if first { first = false; } else { try!(write!(&mut fmt_target, "\n")); }
                    try!(w.format(code.as_slice(), filename, &mut fmt_target));
                }
            },
            ErrEmit(errs) => {
                for err in &errs {
                    match err.detail {
                        None => try!(write!(&mut fmt_target, "emit error: {}", err.msg)),
                        Some(ref detail) => try!(write!(&mut fmt_target, "emit error: {} ({})",
                            err.msg, detail)),
                    };
                }
            }
        },
        Ok(output) => {
            let warns = output.warns;
            if warns.len() > 0 {
                try!(write!(&mut fmt_target, "{} warnings:", warns.len()));
                for w in warns {
                    try!(w.format(code, filename, &mut fmt_target));
                }
            }
        }
    }

    Ok(())
}

fn compile_file(filename: &str) -> io::Result<()> {
    match File::open(&Path::new(filename.clone())) {
        Ok(mut file) => {
            let mut code = String::new();
            match file.read_to_string(&mut code) {
                Ok(_) => {
                    try!(compile(code.as_slice(), filename.as_slice()));
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

    Ok(())
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
        compile(code.as_slice(), "<stdin>").unwrap();
    } else {
        compile_file(f.as_slice()).unwrap();
    }
}
