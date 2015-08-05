//! Lea code formatter.

// FIXME This requires the parser to preserve comments to work properly
// This is blocked on an upstream issue: https://github.com/kevinmehall/rust-peg/issues/84

#![feature(plugin)]
#![plugin(docopt_macros)]

extern crate docopt;
extern crate term;
extern crate rustc_serialize;
extern crate lea_parser as parser;

use parser::span::DummyTerm;
use parser::prettyprint::PrettyPrinter;

use std::io::{self, stdin, stderr, stdout, Read, Write};
use std::fs::File;
use std::path::Path;


docopt!(Args derive Debug, "
Lea source code formatter / pretty printer.

Usage:
    leafmt ( --help | --version )
    leafmt [-o <out>] [--] <file>

Options:
    -h, --help                  Show this help.
    -v, --version               Show the version of the Lea package.
    -o <out>, --out <out>       Write output to <out> (`-` to write to stdout).

By default, leafmt will write the formatted code to stdout.
", flag_out: Option<String>);

/// Opens a terminal that writes to stderr. If stderr couldn't be opened as a terminal, creates a
/// `DummyTerm` that writes to stderr instead.
fn stderr_term() -> Box<term::StderrTerminal> {
    term::stderr().unwrap_or_else(|| Box::new(DummyTerm(io::stderr())))
}

/// Parses the given source code and pretty-prints it
fn prettyprint<W: Write>(code: &str, source_name: &str, mut target: W) -> io::Result<()> {
    match parser::block(code) {
        Ok(main) => {
            let mut pp = PrettyPrinter::new(&mut target);
            try!(pp.print_block(&main));
        }
        Err(e) => {
            try!(e.format(code, source_name, &mut *stderr_term()));
        }
    }

    Ok(())
}

fn read_file(filename: &str) -> io::Result<String> {
    let mut s = String::new();
    let mut file = try!(File::open(&Path::new(filename)));
    try!(file.read_to_string(&mut s));

    Ok(s)
}

#[allow(dead_code)]     // TODO write tests
fn main() {
    match Args::docopt().decode::<Args>() {
        Err(e) => {
            println!("{}", e);
        }
        Ok(args) => {
            match args {
                Args { flag_version: true, .. } => {
                    println!("Lea {}", option_env!("CARGO_PKG_VERSION").unwrap_or("<unknown version>"));
                }
                Args { arg_file, flag_out, .. } => {
                    // Read input
                    let mut code = String::new();
                    let source_name;
                    if arg_file == "-" {
                        stdin().read_to_string(&mut code).unwrap();
                        source_name = "<stdin>".to_owned();
                    } else {
                        source_name = arg_file;
                        code = match read_file(&source_name) {
                            Ok(content) => content,
                            Err(e) => {
                                writeln!(stderr(), "{}", e).unwrap();
                                return;
                            }
                        }
                    }

                    // Open output
                    let out: Box<Write>;
                    match flag_out.as_ref().map(|s| s.as_ref()) {
                        None | Some("-") => {
                            out = Box::new(stdout()) as Box<Write>;
                        }
                        Some(s) => {
                            let f = match File::create(&Path::new(s)) {
                                Ok(f) => f,
                                Err(e) => {
                                    writeln!(stderr(), "{}", e).unwrap();
                                    return;
                                }
                            };

                            out = Box::new(f) as Box<Write>;
                        }
                    }

                    prettyprint(&code, &source_name, out).unwrap();
                }
            }
        }
    }
}
