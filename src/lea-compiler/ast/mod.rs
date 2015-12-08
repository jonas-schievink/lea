//! The Abstract Syntax Tree (AST) used by the Lea compiler.
//!
//! After parsing the source code, the generated parse tree is converted to this simpler AST. The
//! AST is then used for everything else in the compiler.

pub mod visit;
pub mod conv;

use parser::span::{Span, Spanned};
use parser::op::*;

use lea_core::Const;
use lea_core::fndata::UpvalDesc;

use std::collections::HashMap;


/// A block containing any number of statements. All blocks define a scope in which local variables
/// can be declared.
#[derive(Clone, Debug)]
pub struct Block<'a> {
    pub span: Span,
    pub stmts: Vec<Stmt<'a>>,

    /// Maps names of locals declared in this block to their id
    pub localmap: HashMap<&'a str, usize>,
}

impl<'a> Block<'a> {
    /// Create a new block of statements
    pub fn new(stmts: Vec<Stmt<'a>>, span: Span) -> Block<'a> {
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
    #[cfg(test)]    // TODO remove
    pub fn with_locals(stmts: Vec<Stmt<'a>>, span: Span, localmap: HashMap<&'a str, usize>)
    -> Block<'a> {
        Block {
            span: span,
            stmts: stmts,
            localmap: localmap,
        }
    }

    pub fn get_local(&self, name: &str) -> Option<&usize> {
        self.localmap.get(name)
    }
}

impl<'a> PartialEq for Block<'a> {
    /// Compare two `Block`s without comparing their spans
    fn eq(&self, rhs: &Block<'a>) -> bool {
        self.stmts == rhs.stmts && self.localmap == rhs.localmap
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Call<'a> {
    /// Regular call: f(e1, e2, ..)
    Normal(Box<Expr<'a>>, Vec<Expr<'a>>),

    /// some.thing:name(...) - passes `some.thing` as the first argument, without evaluating it
    /// twice
    Method(Box<Expr<'a>>, Spanned<&'a str>, Vec<Expr<'a>>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function<'a> {
    /// Parameters this function takes. Each one also declares a local in the body block.
    pub params: Vec<Spanned<&'a str>>,
    pub varargs: bool,
    pub body: Block<'a>,

    /// Vector of all locals declared in blocks inside this function (multiple with same name
    /// possible). The index into this vector serves as an identification for the local
    pub locals: Vec<Spanned<&'a str>>,
    /// Upvalues referenced by this function. Collected while resolving.
    pub upvalues: Vec<UpvalDesc>,
}

/// Something that can be set to a value
#[derive(Clone, PartialEq, Debug)]
pub enum VarKind<'a> {
    /// References a named variable; later resolved to local, global or upvalue references
    Named(&'a str),

    /// References the local variable with the given ID.
    ///
    /// Note that the resolver has to ensure that the ID is valid, since not all locals can be
    /// reached from all blocks.
    Local(usize),

    /// References the upvalue with the given id (index into the `upvalues` field of the Function)
    Upval(usize),

    /// References an indexed variable (a field)
    Indexed(Box<Variable<'a>>, Box<Expr<'a>>),
}

pub type Variable<'a> = Spanned<VarKind<'a>>;

/// Statement nodes
#[derive(Clone, Debug, PartialEq)]
pub enum StmtKind<'a> {
    /// Declare a list of locals and assign initial values.
    ///
    /// Initial values are optional and default to `nil` (the second vector can have less elements
    /// than the first).
    Decl(Vec<Spanned<&'a str>>, Vec<Expr<'a>>),

    /// Assigns a list of expressions to a list of variables.
    ///
    /// Contains at least one pair. If the last expression is a function or varargs, all return
    /// values are considered for assignment to the leftover variables.
    ///
    /// Might contain less variables than expressions or less expressions than variables. In the
    /// latter case, the leftover variables are assigned to nil.
    Assign(Vec<Variable<'a>>, Vec<Expr<'a>>),

    /// Execute a block in a new scope
    Do(Block<'a>),

    /// Abort the current loop
    Break,

    /// Return a possibly empty list of values to the caller
    Return(Vec<Expr<'a>>),

    /// Function call as statement
    Call(Call<'a>),

    /// Executes `body` if `cond` is true and `el` if not
    If {
        cond: Expr<'a>,
        body: Block<'a>,
        el: Option<Block<'a>>,
    },

    /// Loops a block while `cond` is true
    While {
        cond: Expr<'a>,
        body: Block<'a>,
    },

    /// Loops a block until `abort_on` is true. The body is executed at least once (`abort_on` is
    /// checked after the body has run).
    Repeat {
        body: Block<'a>,
        abort_on: Expr<'a>,
    },

    /// Numeric for loop
    For {
        var: Spanned<&'a str>,    // named local, newly declared
        start: Expr<'a>,
        step: Option<Expr<'a>>,
        end: Expr<'a>,
        body: Block<'a>,
    },

    /// Generic for loop
    ForIn {
        /// The loop variables, returned by iterator
        vars: Vec<Spanned<&'a str>>,
        /// Expression list: Iterator function, state, start value, [ignored ...]
        iter: Vec<Expr<'a>>,
        body: Block<'a>,
    },
}

pub type Stmt<'a> = Spanned<StmtKind<'a>>;

/// Expression nodes
#[derive(Clone, PartialEq, Debug)]
pub enum ExprKind<'a> {
    Lit(Const),
    BinOp(Box<Expr<'a>>, BinOp, Box<Expr<'a>>),
    UnOp(UnOp, Box<Expr<'a>>),

    /// Variable used as expression
    Var(Variable<'a>),
    /// Calls a function, might return multiple results
    Call(Call<'a>),

    /// Instantiates a function/closure
    Func(Function<'a>),

    /// Table constructor
    Table(Vec<(Expr<'a>, Expr<'a>)>),
    /// Array constructor, takes a list of initial values
    Array(Vec<Expr<'a>>),
    /// "..."; expands to var args. only valid if used inside varargs functions
    VarArgs,
}

impl<'a> ExprKind<'a> {
    /// Returns true if this expression might evaluate to multiple results.
    pub fn is_multi_result(&self) -> bool {
        match *self {
            ExprKind::Call(_) | ExprKind::VarArgs => true,
            _ => false,
        }
    }

    /// Returns a boolean that indicates if this expression can have side effects (such as
    /// function invocation, including metamethods, errors, etc.). This is true for most
    /// expressions, but allows the emitter to ignore unused expressions that can't have side
    /// effects.
    pub fn has_side_effects(&self) -> bool {
        match *self {
            ExprKind::Lit(_) | ExprKind::VarArgs | ExprKind::Func(_) => false,
            ExprKind::Var(ref var) => match **var {
                VarKind::Local(_) | VarKind::Upval(_) => false,
                _ => true,  // might cause a table index, which can error
            },
            ExprKind::Array(ref elems) => {
                elems.iter().any(|elem| elem.has_side_effects())
            }
            ExprKind::Table(ref cons) => {
                cons.iter().any(|&(ref k, ref v)| k.has_side_effects() || v.has_side_effects())
            }
            _ => true,
        }
    }
}

pub type Expr<'a> = Spanned<ExprKind<'a>>;
