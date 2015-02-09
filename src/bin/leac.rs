//! The Lea compiler command line frontend

#![feature(io)]
#![feature(path)]
#![feature(env)]
#![feature(core)]
#![feature(os)]

extern crate lea;

use std::env;
use std::fmt;
use std::old_io::{stdio, File};

use lea::parser::parse_main;

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
    println!("Usage: leac file");
    println!("");
    println!("Parses and checks the given file. If an error occurs, outputs it.");
    println!("When the compiler is done, this will actually compile the source code.");
}

fn read_and_compile<T: Reader>(file: &mut T) {
    match file.read_to_string() {
        Ok(source) => {
            let res = parse_main(source.as_slice());

            if let Err(err) = res {
                printerr!("{}", err);
            }
        },
        Err(err) => {
            printerr!("error opening file: {}", err);
        }
    }
}

pub fn main() {
    let mut iter = env::args();
    let mut inputarg: Option<String> = None;
    let mut lastarg: Option<String> = None;

    iter.next();    // Consume first argument (exec name)
    for os_arg in iter {
        let arg = os_arg.into_string().unwrap();

        if let Some(..) = inputarg {
            printerr!("Invalid argument: {}", lastarg.unwrap());
            print_usage();
            return;
        }

        match arg.as_slice() {
            "--help" | "-h" => {
                print_usage();
                return;
            }
            _ => {
                // Unknown arg, must be input file
                inputarg = Some(arg.clone());
            }
        }

        lastarg = Some(arg);
    }

    if let None = inputarg {
        inputarg = Some("-".to_string());   // read stdin by default
    }

    let f = inputarg.unwrap();
    if f.as_slice() == "-" {
        read_and_compile(&mut stdio::stdin_raw());
    } else {
        read_and_compile(&mut File::open(&Path::new(f)));
    }
}
