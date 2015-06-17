//! The Lea compiler command line frontend

#![feature(libc)]

extern crate compiler;
extern crate term;
extern crate libc;

use compiler::*;
use compiler::span::*;

use std::env;
use std::fmt;
use std::default::Default;
use std::io::{self, Read, Write, Stderr};
use std::fs::File;
use std::path::Path;


macro_rules! printerr {
    ($($arg:tt)*) => {
        print_err_fmt(format_args!($($arg)*))
    }
}

fn print_err_fmt(fmt: fmt::Arguments) -> io::Result<()> {
    writeln!(&mut io::stderr(), "{}", fmt)
}

fn print_usage() {
    print!("\
usage: leac [options] <file>

Parses and checks the given file. If file is \"-\", reads stdin. When the compiler is done, this will actually compile the source code.

Options:
 -h, --help     Display this help text
 -v, --version  Display the version of the Lea package
");
}

fn print_version() {
    println!("Lea {}", env!("CARGO_PKG_VERSION"));
}

/// Compiles a piece of code. Prints all errors / warnings to stderr, using color if stderr is a
/// terminal.
fn compile(code: &str, filename: &str) -> io::Result<()> {
    let mut term = term::stderr();
    let stderr = io::stderr();
    let mut dummy = DummyTerm(stderr);
    let mut fmt_target: &mut term::Terminal<Output=Stderr> = if unsafe {
        libc::isatty(libc::STDERR_FILENO) != 0
    } {
        // we can print to the term (or at least try to)
        match term {
            Some(ref mut term) => &mut **term,
            None => &mut dummy,
        }
    } else {
        // use dummy, since we don't want escape sequences
        &mut dummy
    };

    match compile_str(code, filename, &CompileConfig::default()) {
        Err(e) => match e {
            ErrParse(err) => {
                try!(err.format(code.as_ref(), filename, fmt_target));
            },
            ErrCheck(errs) => {
                let mut i = 1;
                for err in &errs {
                    try!(err.format(code.as_ref(), filename, fmt_target));
                    if i < errs.len() - 1 { try!(write!(fmt_target, "\n")); }
                    i += 1;
                }
            },
            ErrLint(warns) => {
                let mut first = true;
                for w in &warns {
                    if first { first = false; } else { try!(write!(fmt_target, "\n")); }
                    try!(w.format(code.as_ref(), filename, fmt_target));
                }
            },
            ErrEmit(errs) => {
                for err in &errs {
                    let mut msg = err.msg.to_string();
                    if let Some(ref d) = err.detail {
                        msg.push_str(format!(" ({})", d).as_ref());
                    }

                    try!(fmt_target.fg(term::color::RED));
                    try!(write!(fmt_target, "{}", msg));
                    try!(fmt_target.reset());
                }
            }
        },
        Ok(output) => {
            let warns = output.warns;
            if warns.len() > 0 {
                for w in warns {
                    try!(w.format(code, filename, fmt_target));
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
                    compile(code.as_ref(), filename.as_ref())
                },
                Err(err) => {
                    printerr!("error reading file \"{}\": {}", filename, err)
                }
            }
        },
        Err(err) => {
            printerr!("error opening file \"{}\": {}", filename, err)
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
            printerr!("invalid argument: {}", lastarg.unwrap()).unwrap();
            print_usage();
            return;
        }

        match arg.as_ref() {
            "-h" | "--help" => {
                print_usage();
                return;
            }
            "-v" | "--version" => {
                print_version();
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
        printerr!("error: no input file").unwrap();
        print_usage();
        return;
    }

    let f_string = inputarg.unwrap();
    let f: &str = f_string.as_ref();
    if f == "-" {
        let mut code = String::new();
        io::stdin().read_to_string(&mut code).unwrap();

        compile(code.as_ref(), "<stdin>").unwrap();
    } else {
        compile_file(f).unwrap();
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        // TODO
    }
}
