//! Contains definitions of the builtin operators supported by Lea.

use std::fmt;

use self::UnOp::*;
use self::BinOp::*;

/// Unary operators
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum UnOp {
    Negate, // -
    LNot,   // !
    LNotLua,// not
    BNot,   // ~
    Len,    // #
}

/// Binary operators
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

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

    Concat,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Assoc {
    Left,
    Right,
}

impl fmt::Display for UnOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_str(match *self {
            Negate => "-",
            LNot => "!",
            LNotLua => "not",
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
            Pow => "^",

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
            BXor => "~",    // compat. to Lua 5.3
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
            Pow => 11,
        }
    }

    pub fn get_assoc(&self) -> Assoc {
        match *self {
            Pow | Concat => Assoc::Right,
            _ => Assoc::Left,
        }
    }
}
