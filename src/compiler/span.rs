//! This module contains the `Span` struct, which represents a range of input characters, along
//! with some helper structs and functions.

use term::Terminal;

use std::fmt;
use std::cmp;
use std::default::Default;
use std::ops::{Deref, DerefMut};
use std::io::{self, Write};

use self::FormatTarget::*;

pub enum FormatTarget<'a, W: Write + 'a> {
    Io(&'a mut W),
    Term(&'a mut Terminal<W>),
}

impl <'a, W: Write> Write for FormatTarget<'a, W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match *self {
            Io(ref mut w) => w.write(buf),
            Term(ref mut t) => t.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match *self {
            Io(ref mut w) => w.flush(),
            Term(ref mut t) => t.flush(),
        }
    }
}

/// A span is a range of input characters in the source code.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub len: usize,
}

impl Span {
    /// Creates a span from `start` to `end` (inclusive).
    pub fn new(start: usize, end: usize) -> Span {
        assert!(start <= end);

        Span {
            start: start,
            len: end - start + 1,   // new(0,0) -> len = 1, start = 0
        }
    }

    /// Given the source code from which this span was created (while compiling it), this prints
    /// the part of the source code this span points to. All lines contained in this span are
    /// printed and below each line, a marker shows which part belongs to the span.
    pub fn format<W: Write>(&self, code: &str, source_name: &str, fmt: &mut FormatTarget<W>) -> io::Result<()> {
        let mut start = self.start;
        let mut len_left = self.len;
        let mut lineno = 1;
        for line in code.lines() {
            if line.len() + 1 > start {
                let prefix = format!("{}:{}   ", source_name, lineno);
                try!(write!(fmt, "{}{}\n", prefix, line));

                for _ in 0..start+prefix.len() {
                    try!(write!(fmt, " "));
                }

                let marks = cmp::min(line.len(), len_left);
                for _ in 0..marks {
                    try!(write!(fmt, "^"));
                }
                len_left -= marks;

                if len_left == 0 { break; }   // whole span printed, done
            }
            start -= cmp::min(line.len() + 1, start);
            lineno += 1;
        }

        Ok(())
    }

    /// Computes the first and last line this span points to in the given source code fragment.
    ///
    /// In most cases, spans are contained to one line, so both values will be the same.
    pub fn get_lines(&self, code: &str) -> (usize, usize) {
        let mut first = 0;
        let mut start = self.start;
        let mut len_left = self.len;
        let mut lineno = 1;
        for line in code.lines() {
            if line.len() + 1 > start {
                if len_left == self.len { first = lineno; }
                len_left -= cmp::min(line.len(), len_left);

                if len_left == 0 { break; }   // whole span printed, done
            }
            start -= cmp::min(line.len() + 1, start);
            lineno += 1;
        }

        (first, lineno)
    }
}

impl Default for Span {
    fn default() -> Span {
        Span::new(0, 0)     // first char only (length: 1)
    }
}

/// Wraps a T and a Span.
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl <T> Spanned<T> {
    pub fn new(span: Span, t: T) -> Spanned<T> {
        Spanned {
            span: span,
            value: t,
        }
    }

    pub fn default(t: T) -> Spanned<T> {
        Spanned::new(Default::default(), t)
    }
}

impl <T> Deref for Spanned<T> {
    type Target = T;

    fn deref<'a>(&'a self) -> &'a T {
        &self.value
    }
}

impl <T> DerefMut for Spanned<T> {
    fn deref_mut<'a>(&'a mut self) -> &'a mut T {
        &mut self.value
    }
}

impl <T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Spanned<T> {
        Spanned {
            span: self.span,
            value: self.value.clone(),
        }
    }
}

impl <T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.value.fmt(fmt)
    }
}

impl <T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Spanned<T>) -> bool {
        self.value == other.value
    }
}

/// Helper method that creates a Spanned<T> that extends from `start` until the character before
/// `end` (unlike `Span::new`).
/// Used by the PEG parser.
pub fn mkspanned<T>(t: T, start: usize, end: usize) -> Spanned<T> {
    //assert!(start < end);
    Spanned::new(Span::new(start, end - 1), t)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test(span: Span, code: &str, expect: &str) {
        let mut v = Vec::<u8>::new();

        {
            let mut fmt = FormatTarget::Io(&mut v);
            span.format(code, "A", &mut fmt).unwrap();
        }

        assert_eq!(v.as_slice(), expect.as_bytes());
    }

    #[test]
    fn span_format() {
        test(Span::new(0, 0), "Aaa", "A:1   Aaa\n      ^");
        test(Span::new(3, 3), "a\naAa", "A:2   aAa\n       ^");
        test(Span::new(1, 1), "aA", "A:1   aA\n       ^");
    }

    #[test] #[should_panic]
    fn span_inv() {
        // span outside of source code
        test(Span::new(4, 4), "aaa", "A:1   aaa\n         ^");
    }
}
