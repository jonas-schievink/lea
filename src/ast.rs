pub use self::_Variable::*;
pub use self::_Stmt::*;
pub use self::_Expr::*;
pub use self::Literal::*;

use std::fmt;

use span::Spanned;

use self::UnOp::*;
use self::BinOp::*;

/// Literal constants
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    TInt(i64),
    TFloat(f64),
    TStr(String),
    TBool(bool),
    TNil,
}

/// Unary operators
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UnOp {
    Negate, // -
    LNot,   // !
    BNot,   // ~
    Len,    // #
}

/// Binary operators
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Eq,
    NEq,
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
            Eq | NEq | LEq | GEq | Less | Greater => 0,
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

/// A block containing any number of statements. All blocks carry a scope in which local variables
/// can be declared.
#[derive(Clone, Debug)]
pub struct _Block {
    pub stmts: Vec<Stmt>,

    /// List of locals. Collected when resolving.
    pub locals: Vec<String>,
}

impl _Block {
    pub fn new(stmts: Vec<Stmt>) -> _Block {
        _Block {
            stmts: stmts,
            locals: vec![],
        }
    }

    pub fn with_locals(stmts: Vec<Stmt>, locals: Vec<String>) -> _Block {
        _Block {
            stmts: stmts,
            locals: locals,
        }
    }
}

impl PartialEq for _Block {
    fn eq(&self, other: &_Block) -> bool {
        use std::collections::HashSet;

        // Compares locals ignoring their order.
        if self.stmts == other.stmts {
            let mut set = HashSet::new();
            for l in &self.locals {
                set.insert(l);
            }

            for l in &other.locals {
                if !set.contains(l) { return false; }
            }

            true
        } else {
            false
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Call {
    pub callee: Box<Expr>,
    pub argv: Vec<Expr>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct _Function {
    // Parameters this function takes. Each one declares a similarly named local in the body block.
    pub params: Vec<String>,
    pub varargs: bool,
    pub body: Block,
}

/// Something that can be assigned to a value
#[derive(Clone, PartialEq, Debug)]
pub enum _Variable {
    /// References a named variable; later resolved to local or global
    VNamed(String),

    /// References the local variable in the current scope with the given name
    VLocal(String),

    /// References a local declared by the parent function
    VUpvalue(String),

    /// References a named global
    VGlobal(String),

    /// References an indexed variable (a field)
    VIndex(Box<Variable>, Box<Expr>),

    /// References a variable indexed with dot notation
    VDotIndex(Box<Variable>, String),
}

/// Statement nodes
#[derive(Clone, Debug, PartialEq)]
pub enum _Stmt {
    /// Declare a list of locals and assign initial values.
    ///
    /// Initial values are optional and default to `nil` (the second vector can have less elements
    /// than the first).
    SDecl(Vec<String>, Vec<Expr>),

    /// Assigns a list of expressions to a list of variables.
    ///
    /// Contains at least one pair. If the last expression is a function or varargs, all return
    /// values are considered for assignment to the leftover variables.
    ///
    /// Might contain less variables than expressions or less expressions than variables. In the
    /// latter case, the leftover variables are assigned to nil.
    SAssign(Vec<Variable>, Vec<Expr>),

    /// Execute a block in a new scope
    SDo(Block),

    /// Abort the current loop
    SBreak,

    /// Return a possibly empty list of values to the caller
    SReturn(Vec<Expr>),

    /// Function call as statement
    SCall(Call),

    /// Assign function to named variable (`function XY(...) ... end`)
    SFunc(Variable, Function),

    /// Assign function to named local (`local function XY(...) ... end`)
    SLFunc(String, Function),

    /// Executes `body` if `cond` is true and `el` if not
    SIf {
        cond: Expr,
        body: Block,
        el: Block,
    },

    /// Loops a block while `cond` is true
    SWhile {
        cond: Expr,
        body: Block,
    },

    /// Loops a block until `abort_on` is true
    SRepeat {
        abort_on: Expr,
        body: Block,
    },

    /// Numeric for loop
    SFor {
        var: String,    // named local
        start: Expr,
        step: Expr,
        end: Expr,
        body: Block,
    },

    /// Generic for loop
    SForIn {
        /// The loop variables, returned by iterator
        vars: Vec<String>,
        /// Expression list: Iterator function, invariant state, start value, [ignored ...]
        iter: Vec<Expr>,
        body: Block,
    },
}

/// Expression nodes
#[derive(Clone, PartialEq, Debug)]
pub enum _Expr {
    ELit(Literal),
    EBinOp(Box<Expr>, BinOp, Box<Expr>),
    EUnOp(UnOp, Box<Expr>),

    /// Raw binary expression returned from generated parser. Operator precedences are not yet
    /// applied, since the generated parser doesn't know about them.
    ///
    /// These expression are turned into EBinOp's right after the PEG-generated parser is run, so
    /// following code only has to deal with tree-like expressions.
    ERawOp(Box<Expr>, Vec<(BinOp, Expr)>),

    EVar(Variable),
    ECall(Call),

    /// Instantiates a function/closure
    EFunc(Function),

    /// Table constructor, takes key-value pairs
    ETable(Vec<(Expr, Expr)>),
    /// Array constructor, takes a list of initial values
    EArray(Vec<Expr>),
    /// "..."; expands to var args. only valid if used inside varargs functions
    EVarArgs,
}

pub type Expr = Spanned<_Expr>;
pub type Stmt = Spanned<_Stmt>;
pub type Block = Spanned<_Block>;
pub type Function = Spanned<_Function>;
pub type Variable = Spanned<_Variable>;
