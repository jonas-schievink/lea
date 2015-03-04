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

    Concat,

    Eq,
    NEq,
    NEqLua,
    LEq,
    GEq,
    Less,
    Greater,

    LAnd,
    LAndLua,
    LOr,
    LOrLua,
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

            Concat => "..",

            Eq => "==",
            NEq => "!=",
            NEqLua => "~=",
            LEq => "<=",
            GEq => ">=",
            Less => "<",
            Greater => ">",

            LAnd => "&&",
            LAndLua => "and",
            LOr => "||",
            LOrLua => "or",
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
            LOr | LOrLua => 0,
            LAnd | LAndLua => 1,
            Eq | NEq | NEqLua | LEq | GEq | Less | Greater => 2,
            Concat => 3,
            BAnd => 4,
            BXor => 5,
            BOr => 6,
            ShiftL => 7,
            ShiftR => 8,
            Add | Sub => 9,
            Mul | Div | Mod => 10,
        }
    }
}
