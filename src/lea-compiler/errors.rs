//! Defines compiler errors, warnings and printing methods.

use super::emitter::EmitError;
use super::parser::ParseError;
use super::check::CheckError;

use parser::span::Span;

use term::{color, Terminal};

use std::io::{self, Write};

pub use self::CompileError::*;

/// Kinds of errors that can occur when compiling code
#[derive(Debug)]
pub enum CompileError {
    /// The input source code could not be parsed
    ErrParse(ParseError),

    /// The checker encountered one or more problems
    ErrCheck(Vec<CheckError>),

    /// One or more Lints have issued warnings and were configured to cause a compilation error.
    /// Contains all warnings of all Lints that were configured to error.
    ErrLint(Vec<Warning>),

    /// Byte code emission failed. This can happen if an implementation limit is reached.
    ErrEmit(Vec<EmitError>),
}

impl From<ParseError> for CompileError {
    fn from(e: ParseError) -> Self {
        ErrParse(e)
    }
}

impl From<Vec<CheckError>> for CompileError {
    fn from(errs: Vec<CheckError>) -> Self {
        ErrCheck(errs)
    }
}

impl CompileError {
    pub fn format<W: Write>(&self, code: &str, source_name: &str, t: &mut Terminal<Output=W>)
    -> io::Result<()> {
        match *self {
            ErrParse(ref err) => {
                try!(err.format(code, source_name, t));
            },
            ErrCheck(ref errs) => {
                let mut i = 1;
                for err in errs {
                    try!(err.format(code, source_name, t));
                    if i < errs.len() - 1 { try!(write!(t, "\n")); }
                    i += 1;
                }
            },
            ErrLint(ref warns) => {
                let mut first = true;
                for w in warns {
                    if first { first = false; } else { try!(write!(t, "\n")); }
                    try!(w.format(code, source_name, t));
                }
            },
            ErrEmit(ref errs) => {
                for err in errs {
                    let mut msg = err.msg.to_owned();
                    if let Some(ref d) = err.detail {
                        msg.push_str(&format!(" ({})", d));
                    }

                    try!(t.fg(color::RED));
                    try!(write!(t, "{}", msg));
                    try!(t.reset());
                }
            }
        }

        Ok(())
    }
}

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Clone, Debug)]
pub struct Warning {
    span: Span,
    message: String,
    /// Additional help text attached to the message (one per line)
    info: Vec<String>,
}

impl Warning {
    pub fn new(span: Span, message: String) -> Warning {
        Warning::with_info(span, message, Vec::new())
    }

    pub fn with_info(span: Span, message: String, info: Vec<String>) -> Warning {
        Warning {
            span: span,
            message: message,
            info: info,
        }
    }

    /// Formats this warning, its span, and all attached info lines. Does not append a trailing
    /// newline.
    pub fn format<W: Write>(&self, code: &str, source_name: &str, t: &mut Terminal<Output=W>)
    -> io::Result<()> {
        let (startline, _end) = self.span.get_lines(code);
        try!(self.span.print_with_warn(code, source_name, &self.message, t));

        for info in &self.info {
            try!(Span::print_info(source_name, startline, None, info, t));
        }

        Ok(())
    }

    pub fn get_message(&self) -> &str {
        &self.message
    }
}
