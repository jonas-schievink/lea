pub use self::_Variable::*;
pub use self::_Stmt::*;
pub use self::_Expr::*;
pub use self::Literal::*;

use std::fmt;
use std::collections::HashMap;
use std::default::Default;

use super::span::{Span, Spanned};
use program::UpvalDesc;
use op::*;

/// Literal constants
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    TInt(i64),
    TFloat(f64),
    TStr(String),
    TBool(bool),
    TNil,
}

/// A block containing any number of statements. All blocks define a scope in which local variables
/// can be declared.
#[derive(Clone, Debug)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,

    /// Maps names of locals declared in this block to their id
    pub localmap: HashMap<String, usize>,
}

impl Block {
    /// Create a new block of statements
    pub fn new(stmts: Vec<Stmt>, span: Span) -> Block {
        Block {
            span: span,
            stmts: stmts,
            localmap: Default::default(),
        }
    }

    /// Creates a new block and assigns a map of locals declared inside this block.
    ///
    /// Note that this does not check if the local map is valid. This would require access to the
    /// enclosing Function.
    pub fn with_locals(stmts: Vec<Stmt>, span: Span, localmap: HashMap<String, usize>) -> Block {
        Block {
            span: span,
            stmts: stmts,
            localmap: localmap,
        }
    }
}

impl PartialEq for Block {
    /// Compare two `Block`s without comparing their spans
    fn eq(&self, rhs: &Block) -> bool {
        self.stmts == rhs.stmts && self.localmap == rhs.localmap
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Call {
    pub callee: Box<Expr>,
    pub argv: Vec<Expr>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct _Function {
    /// Parameters this function takes. Each one also declares a local in the body block.
    pub params: Vec<String>,
    /// Vector of all locals declared in blocks inside this function (multiple with same name
    /// possible). The index into this vector serves as an identification for the local
    pub locals: Vec<String>,
    pub varargs: bool,
    pub body: Block,
    /// Upvalues referenced by this function. Collected while resolving.
    pub upvalues: Vec<UpvalDesc>,
}

impl _Function {
    pub fn new(params: Vec<String>, varargs: bool, body: Block) -> _Function {
        _Function {
            params: params,
            locals: vec![],
            varargs: varargs,
            body: body,
            upvalues: vec![],
        }
    }
}

/// Something that can be assigned to a value
#[derive(Clone, PartialEq, Debug)]
pub enum _Variable {
    /// References a named variable; later resolved to local, global or upvalue references
    VNamed(String),

    /// References the local variable with the given ID.
    ///
    /// Note that the resolver has to ensure that the usize is valid, since not all locals can be
    /// reached from all blocks.
    VLocal(usize),

    /// References the upvalue with the given id (index into the `upvalues` field of the Function)
    VUpval(usize),

    /// References a named global
    VGlobal(String),

    /// References a resolved global. The left variable is the environment, which is indexed with
    /// the string on the right.
    VResGlobal(Box<Variable>, String),

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

    /// Assign function to newly declared local (`local function XY(...) ... end`)
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
pub type Function = Spanned<_Function>;
pub type Variable = Spanned<_Variable>;
