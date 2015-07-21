//! Parse tree -> AST conversion

use super::Function;
use parser::parsetree;

use std::convert::From;

impl<'a> From<parsetree::Function<'a>> for Function<'a> {
    fn from(func: parsetree::Function<'a>) -> Self {
        unimplemented!();
    }
}
