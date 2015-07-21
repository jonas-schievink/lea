//! Parse tree -> AST conversion

use super::{Block, Function};
use parser::parsetree;

use std::convert::From;

impl<'a> From<parsetree::Function<'a>> for Function<'a> {
    fn from(_func: parsetree::Function<'a>) -> Self {
        unimplemented!();
    }
}

impl<'a> From<parsetree::Block<'a>> for Block<'a> {
    fn from(_block: parsetree::Block<'a>) -> Self {
        unimplemented!();
    }
}
