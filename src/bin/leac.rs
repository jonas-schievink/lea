//! The `leac` compiler command line utility.

#![feature(plugin)]
#![plugin(docopt_macros)]

extern crate docopt;
extern crate rustc_serialize;
extern crate bincode;
extern crate term;
extern crate lea_compiler as compiler;
extern crate lea_parser as parser;
extern crate lea;

use parser::span::DummyTerm;
use compiler::{CompileConfig, FnData};

use rustc_serialize::json;
use bincode::{EncodingError, SizeLimit};

use std::io::{self, stdin, stderr, stdout, Read, Write};
use std::fs::File;
use std::path::Path;


#[derive(Debug, RustcDecodable)]
#[allow(non_camel_case_types)]
enum Encoding {
    json,
    bin,
    debug,
}

impl Encoding {
    pub fn encode<W: Write>(&self, fndata: &FnData, wr: &mut W)
    -> io::Result<()> {
        match *self {
            Encoding::json => {
                match json::encode(fndata) {
                    Ok(s) => Ok(try!(write!(wr, "{}", &s))),
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "json encoding error")),
                }
            }
            Encoding::bin => {
                match bincode::encode_into(fndata, wr, SizeLimit::Infinite) {
                    Ok(_) => Ok(()),
                    Err(EncodingError::IoError(e)) => Err(e),
                    Err(EncodingError::SizeLimit) => Err(io::Error::new(io::ErrorKind::Other, "size limit reached")),
                }
            }
            Encoding::debug => {
                write!(wr, "{:#?}\n", fndata)
            }
        }
    }
}

docopt!(Args derive Debug, "
The Lea compiler

Usage:
    leac ( --help | --version )
    leac [-o <out>] [-e <enc>] [--] <file>
    leac ( -p | --pretty ) [--] <file>

Options:
    -h, --help                  Show this help.
    -v, --version               Show the version of the Lea package.
    -p, --pretty                Pretty print the source code instead of compiling it.
    -o <out>, --out <out>       Write output to <out> (`-` to write to stdout).
    -e <enc>, --encoding <enc>  The encoding to use for writing the compiled code.

By default, leac will write the compiled code to stdout. If stdout is a terminal, leac will print \
it using the `debug` encoding. Otherwise, leac will encode the compilation result in a binary \
form (`bin` encoding).
", flag_encoding: Option<Encoding>, flag_out: Option<String>);


/// Opens a terminal that writes to stderr. If stderr couldn't be opened as a terminal, creates a
/// `DummyTerm` that writes to stderr instead.
fn stderr_term() -> Box<term::StderrTerminal> {
    term::stderr().unwrap_or_else(|| Box::new(DummyTerm(io::stderr())))
}

/// Compiles a piece of code. Prints all errors / warnings to stderr, using color if stderr is a
/// terminal.
fn compile(code: &str, filename: &str) -> io::Result<Option<FnData>> {
    let mut fmt_target = stderr_term();
    let fmt_target = &mut *fmt_target;

    match compiler::compile_str(code, filename, &CompileConfig::default()) {
        Err(e) => {
            try!(e.format(code, filename, fmt_target));
            Ok(None)
        },
        Ok(output) => {
            let warns = output.warns;
            if warns.len() > 0 {
                for w in warns {
                    try!(w.format(code, filename, fmt_target));
                }
            }

            Ok(Some(output.mainproto))
        }
    }
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
                    println!("Lea {}", lea::version_str());
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
                    let mut out: Box<Write>;
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

                    let enc = args.flag_encoding.unwrap_or(Encoding::bin);

                    match compile(&code, &source_name).unwrap() {
                        Some(fndata) => {
                            // serialize to `out`, except if `out` is a terminal and we defaulted to `-` (TODO)
                            match enc.encode(&fndata, &mut out) {
                                Err(e) => {
                                    writeln!(stderr(), "{}", e).unwrap();
                                    return;
                                }
                                _ => {}
                            }
                        }
                        None => {}
                    }
                }
            }
        }
    }
}
