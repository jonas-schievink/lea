//! Lea code formatter.

// FIXME This requires the parser to preserve comments to work properly
// This is blocked on an upstream issue: https://github.com/kevinmehall/rust-peg/issues/84

#[macro_use]
extern crate clap;
extern crate term;
extern crate rustc_serialize;
extern crate lea_parser as parser;
extern crate lea;

use parser::span::DummyTerm;
use parser::prettyprint::PrettyPrinter;

use std::io::{self, stdin, stderr, stdout, Read, Write};
use std::fs::File;
use std::path::Path;


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

fn main() {
    let matches = clap_app!(leafmt =>
        (version: lea::version_str())
        (about: "Lea source code formatter / pretty printer")
        (@arg FILE: +required "The file to format (`-` to read from stdin)")
        (@arg out: -o --out +takes_value "Write output to <out> (`-` to write to stdout).")
        (after_help: "By default, leafmt will write the formatted code to stdout.\n")
    ).get_matches();

    let file = matches.value_of("FILE").unwrap();

    // Read input
    let mut code = String::new();
    let source_name;
    if file == "-" {
        stdin().read_to_string(&mut code).unwrap();
        source_name = "<stdin>";
    } else {
        source_name = file;
        code = match read_file(&source_name) {
            Ok(content) => content,
            Err(e) => {
                writeln!(stderr(), "{}", e).unwrap();
                return;
            }
        }
    }

    // Open output
    let writer: Box<Write>;
    match matches.value_of("out") {
        None | Some("-") => {
            writer = Box::new(stdout()) as Box<Write>;
        }
        Some(s) => {
            let f = match File::create(&Path::new(s)) {
                Ok(f) => f,
                Err(e) => {
                    writeln!(stderr(), "{}", e).unwrap();
                    return;
                }
            };

            writer = Box::new(f) as Box<Write>;
        }
    }

    prettyprint(&code, &source_name, writer).unwrap();
}
