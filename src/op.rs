//! Contains definitions of the builtin operators supported by Lea.

use std::fmt;

pub use self::UnOp::*;
pub use self::BinOp::*;

/// Unary operators
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum UnOp {
    Negate, // -
    LNot,   // !
    BNot,   // ~
    Len,    // #
}

/// Binary operators
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Eq,
    NEq,
    NEqLua,
    LEq,
    GEq,
    Less,
    Greater,

    LAnd,
    LOr,
    BAnd,
    BOr,
    BXor,
    ShiftL,
    ShiftR,
}

impl fmt::Display for UnOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_str(match *self {
            Negate => "-",
            LNot => "!",
            BNot => "~",
            Len => "#",
        })
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_str(match *self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",

            Eq => "==",
            NEq => "!=",
            NEqLua => "~=",
            LEq => "<=",
            GEq => ">=",
            Less => "<",
            Greater => ">",

            LAnd => "&&",
            LOr => "||",
            BAnd => "&",
            BOr => "|",
            BXor => "^",
            ShiftL => "<<",
            ShiftR => ">>",
        })
    }
}

impl BinOp {
    pub fn get_precedence(&self) -> u8 {
        match *self {
            Eq | NEq | NEqLua | LEq | GEq | Less | Greater => 0,
            LOr | LAnd => 1,
            BAnd => 2,
            BXor => 3,
            BOr => 4,
            ShiftL => 5,
            ShiftR => 6,
            Add | Sub => 7,
            Mul | Div | Mod => 8,
        }
    }
}
