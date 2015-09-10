//! This module contains the `Span` struct, which represents a range of input characters, along
//! with some helper structs and functions.

use term::{Terminal, Attr};
use term::color::{self, Color};

use unicode_segmentation::UnicodeSegmentation;

use std::fmt;
use std::cmp;
use std::ops::{Deref, DerefMut};
use std::io::{self, Write};

const WARN_COLOR: Color = color::BRIGHT_YELLOW;
const ERR_COLOR: Color = color::BRIGHT_RED;
const INFO_COLOR: Color = color::CYAN;

/// Wraps a `Write` and implements `Terminal`
pub struct DummyTerm<W: Write>(pub W);

impl<W: Write> Terminal for DummyTerm<W> {
    type Output = W;

    fn fg(&mut self, _color: Color) -> io::Result<bool> {
        Ok(false)
    }

    fn bg(&mut self, _color: Color) -> io::Result<bool> {
        Ok(false)
    }

    fn attr(&mut self, _attr: Attr) -> io::Result<bool> {
        Ok(false)
    }

    fn supports_attr(&self, _attr: Attr) -> bool {
        false
    }

    fn reset(&mut self) -> io::Result<bool> {
        Ok(false)
    }

    fn cursor_up(&mut self) -> io::Result<bool> {
        Ok(false)
    }

    fn delete_line(&mut self) -> io::Result<bool> {
        Ok(false)
    }

    fn carriage_return(&mut self) -> io::Result<bool> {
        Ok(false)
    }

    fn get_ref(&self) -> &W {
        &self.0
    }

    fn get_mut(&mut self) -> &mut W {
        &mut self.0
    }

    fn into_inner(self) -> W where Self: Sized {
        self.0
    }
}

impl<W: Write> Write for DummyTerm<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
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

    /// Prints the file name, line and (optionally) column. Returns the number of graphemes
    /// written (to allow proper indentation).
    pub fn print_loc<W: Write>(source_name: &str, line: usize, col: Option<usize>, t: &mut Terminal<Output=W>) -> io::Result<usize> {
        let mut s = format!("{}:{}:", source_name, line);
        if let Some(col) = col { s.push_str(&format!("{}:", col)); }
        s.push_str(" ");
        try!(t.reset());
        try!(write!(t, "{}", s));

        Ok(s.graphemes(true).count())
    }

    /// Prints a string with code formatting, followed by a line break
    fn print_code<W: Write>(code: &str, t: &mut Terminal<Output=W>) -> io::Result<()> {
        try!(t.reset());
        try!(write!(t, "{}\n", code));
        Ok(())
    }

    /// Prints a message followed by a line break
    fn print_msg<W: Write>(msg: &str, t: &mut Terminal<Output=W>) -> io::Result<()> {
        try!(t.reset());
        try!(t.attr(Attr::Bold));
        try!(write!(t, "{}\n", msg));
        try!(t.reset());
        Ok(())
    }

    /// Given the source code from which this span was created, this prints the part of the source
    /// code this span points to. All lines contained in this span are printed and below each line,
    /// a marker shows which part belongs to the span.
    pub fn format<W: Write>(&self, code: &str, source_name: &str, t: &mut Terminal<Output=W>, c: Color)
    -> io::Result<()> {
        let mut start = self.start;
        let mut len_left = self.len;
        let mut lineno = 1;
        for line in code.lines() {
            if line.len() + 1 > start {
                let prefixlen = try!(Span::print_loc(source_name, lineno, None, t));
                try!(Span::print_code(line, t));

                for _ in 0..start+prefixlen {
                    try!(write!(t, " "));
                }

                try!(t.fg(c));
                let marks = cmp::min(line.len(), len_left);
                for _ in 0..marks {
                    try!(write!(t, "^"));
                }
                try!(t.reset());
                len_left -= marks;

                if len_left == 0 { break; }   // whole span printed, done
            }
            start -= cmp::min(line.len() + 1, start);
            lineno += 1;
        }

        try!(write!(t, "\n"));
        Ok(())
    }

    /// Prints an error message with file position info, followed by this span (and the source line
    /// it points to).
    pub fn print_with_err<W: Write>(&self, code: &str, source_name: &str, err: &str, t: &mut Terminal<Output=W>)
    -> io::Result<()> {
        let (start, _end) = self.get_lines(code);
        try!(Span::print_loc(source_name, start, None, t));
        try!(t.fg(ERR_COLOR));
        try!(write!(t, "error: "));
        try!(Span::print_msg(err, t));
        try!(self.format(code, source_name, t, ERR_COLOR));
        Ok(())
    }

    pub fn print_with_warn<W: Write>(&self, code: &str, source_name: &str, warn: &str, t: &mut Terminal<Output=W>)
    -> io::Result<()> {
        let (start, _end) = self.get_lines(code);
        try!(Span::print_loc(source_name, start, None, t));
        try!(t.fg(WARN_COLOR));
        try!(write!(t, "warning: "));
        try!(Span::print_msg(warn, t));
        try!(self.format(code, source_name, t, WARN_COLOR));
        Ok(())
    }

    pub fn print_info<W: Write>(source_name: &str, line: usize, col: Option<usize>, info: &str,
    t: &mut Terminal<Output=W>) -> io::Result<()> {
        try!(Span::print_loc(source_name, line, col, t));
        try!(t.fg(INFO_COLOR));
        try!(write!(t, "info: "));
        try!(Span::print_msg(info, t));
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

                if len_left == 0 { break; }
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
#[derive(Copy, Clone)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
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

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.value.fmt(fmt)
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
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
    use term::color;

    fn test(span: Span, code: &str, expect: &str) {
        let mut v = Vec::<u8>::new();

        {
            let mut fmt = DummyTerm(&mut v);
            span.format(code, "A", &mut fmt, color::RED).unwrap();
        }

        assert_eq!(String::from_utf8(v).unwrap(), expect);
    }

    #[test]
    fn span_format() {
        test(Span::new(0, 0), "Aaa", "A:1: Aaa\n     ^\n");
        test(Span::new(3, 3), "a\naAa", "A:2: aAa\n      ^\n");
        test(Span::new(1, 1), "aA", "A:1: aA\n      ^\n");
    }

    #[test] #[should_panic]
    fn span_inv() {
        // span outside of source code
        test(Span::new(4, 4), "aaa", "A:1: aaa\n        ^\n");
    }
}
