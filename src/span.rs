//! This module contains the `Span` struct, which represents a range of input characters, along
//! with some helper structs and functions.

use std::fmt;
use std::num::Int;
use std::default::Default;
use std::ops::{Deref, DerefMut};

/// A span is a range of input characters in the source code. Span does not know about lines and
/// the PEG parser handles lines as whitespace, so it is not trivial to convert a Span object (a
/// span of characters) into a span of lines.
#[derive(Copy, Debug, PartialEq)]
pub struct Span {
    pub start: usize,
    pub len: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span {
            start: start,
            len: end - start,
        }
    }
}

impl Default for Span {
    fn default() -> Span {
        Span::new(Int::max_value(), Int::max_value())
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

/// Helper method that creates a Spanned<T>
/// Used by the PEG parser.
pub fn mkspanned<T>(t: T, start: usize, end: usize) -> Spanned<T> {
    Spanned::new(Span::new(start, end), t)
}
