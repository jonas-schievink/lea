//! The `leac` compiler command line utility.

#[macro_use] extern crate clap;
extern crate env_logger;
extern crate rustc_serialize;
extern crate bincode;
extern crate term;
extern crate lea_compiler as compiler;
extern crate lea_parser as parser;
extern crate lea;

use parser::span::DummyTerm;
use compiler::{CompileConfig, FnData};

use rustc_serialize::json;
use bincode::SizeLimit;
use bincode::rustc_serialize::EncodingError;

use std::io::{self, stdin, stderr, stdout, Read, Write};
use std::fs::File;
use std::path::Path;
use std::str::FromStr;


#[derive(Debug)]
#[allow(non_camel_case_types)]
enum Encoding {
    Json,
    Bin,
    Debug,
}

impl Encoding {
    pub fn encode<W: Write>(&self, fndata: &FnData, wr: &mut W)
    -> io::Result<()> {
        use Encoding::*;
        match *self {
            Json => {
                match json::encode(fndata) {
                    Ok(s) => Ok(try!(write!(wr, "{}", &s))),
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "json encoding error")),
                }
            }
            Bin => {
                match bincode::rustc_serialize::encode_into(fndata, wr, SizeLimit::Infinite) {
                    Ok(_) => Ok(()),
                    Err(EncodingError::IoError(e)) => Err(e),
                    Err(EncodingError::SizeLimit) => Err(io::Error::new(io::ErrorKind::Other, "size limit reached")),
                }
            }
            Debug => {
                write!(wr, "{:#?}\n", fndata)
            }
        }
    }
}

impl FromStr for Encoding {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Encoding::*;
        match s {
            "json" => Ok(Json),
            "bin" => Ok(Bin),
            "debug" => Ok(Debug),
            _ => Err(()),
        }
    }
}


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
            if !warns.is_empty() {
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

fn main() {
    env_logger::init().unwrap();

    let matches = clap_app!(leac =>
        (version: lea::version_str())
        (about: "The Lea byte code compiler")
        (@arg FILE: +required "The file to compile")
        (@arg out: -o --out +takes_value "Write output to <out> (`-` to write to stdout).")
        (@arg enc: -e --encoding +takes_value "The encoding to use for writing the compiled code.")
        (after_help: "By default, leac will write the compiled code to stdout. If stdout is a \
            terminal, leac will print it using the `debug` encoding. Otherwise, leac will encode \
            the compilation result in a binary form (`bin` encoding).\n")
    ).get_matches();

    let encoding = match matches.value_of("enc") {
        None => Encoding::Debug,
        Some(_) => value_t_or_exit!(matches.value_of("enc"), Encoding),
    };

    // Read input
    let file = matches.value_of("FILE").unwrap();
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
    let mut writer: Box<Write>;
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

    match compile(&code, &source_name).unwrap() {
        Some(fndata) => {
            // serialize to `out`, except if `out` is a terminal and we defaulted to `-` (TODO)
            if let Err(e) = encoding.encode(&fndata, &mut writer) {
                writeln!(stderr(), "{}", e).unwrap();
                return;
            }
        }
        None => {}
    }
}
