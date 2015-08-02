//! Contains the parse tree generated by the parser

// TODO: Represent comments in the parse tree

pub use self::_Variable::*;
pub use self::_Stmt::*;
pub use self::_Expr::*;
pub use self::Call::*;

use span::{Span, Spanned};
use op::*;

use lea_core::literal::*;


/// A block containing any number of statements. All blocks define a scope in which local variables
/// can be declared.
#[derive(Clone, Debug, Default)]
pub struct Block<'a> {
    pub span: Span,
    pub stmts: Vec<Stmt<'a>>,
}

impl<'a> PartialEq for Block<'a> {
    /// Compare two `Block`s without comparing their spans
    fn eq(&self, rhs: &Block<'a>) -> bool {
        self.stmts == rhs.stmts
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum CallArgs<'a> {
    Normal(Vec<Expr<'a>>),
    String(String),
    Table(TableCons<'a>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Call<'a> {
    /// Regular call: f(e1, e2, ..)
    SimpleCall(Box<Expr<'a>>, CallArgs<'a>),

    /// some.thing:name(...) - passes `some.thing` as the first argument, without evaluating it
    /// twice
    MethodCall(Box<Expr<'a>>, Spanned<&'a str>, CallArgs<'a>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function<'a> {
    /// Parameters this function takes. Each one also declares a local in the body block.
    pub params: Vec<Spanned<&'a str>>,
    pub varargs: bool,
    pub body: Block<'a>,
}

impl<'a> Function<'a> {
    pub fn new(params: Vec<Spanned<&'a str>>, varargs: bool, body: Block<'a>) -> Function<'a> {
        Function {
            params: params,
            varargs: varargs,
            body: body,
        }
    }
}

/// Describes how a variable is indexed
#[derive(Clone, PartialEq, Debug)]
pub enum VarIndex<'a> {
    DotIndex(Spanned<&'a str>),
    ExprIndex(Box<Expr<'a>>),
}

/// Something that can be set to a value
#[derive(Clone, PartialEq, Debug)]
pub enum _Variable<'a> {
    /// References a named variable; later resolved to local, global or upvalue references
    VNamed(&'a str),

    VIndex(Box<Variable<'a>>, VarIndex<'a>),
}

/// Statement nodes
#[derive(Clone, Debug, PartialEq)]
pub enum _Stmt<'a> {
    /// Declare a list of locals and assign initial values.
    ///
    /// Initial values are optional and default to `nil` (the second vector can have less elements
    /// than the first).
    SDecl(Vec<Spanned<&'a str>>, Vec<Expr<'a>>),

    /// Assigns a list of expressions to a list of variables.
    ///
    /// Contains at least one pair. If the last expression is a function or varargs, all return
    /// values are considered for assignment to the leftover variables.
    ///
    /// Might contain less variables than expressions or less expressions than variables. In the
    /// latter case, the leftover variables are assigned to nil.
    SAssign(Vec<Variable<'a>>, Vec<Expr<'a>>),

    /// Execute a block in a new scope
    SDo(Block<'a>),

    /// Abort the current loop
    SBreak,

    /// Return a possibly empty list of values to the caller
    SReturn(Vec<Expr<'a>>),

    /// Function call as statement
    SCall(Call<'a>),

    /// Assign function to named variable.
    ///
    /// ```lua
    /// function XY(...) ... end
    /// ```
    ///
    /// is equivalent to
    ///
    /// ```lua
    /// XY = function(...) ... end
    /// ```
    SFunc(Variable<'a>, Function<'a>),

    /// Method declaration.
    ///
    /// ```lua
    /// function some.thing:methodname(...) ... end
    /// ```
    ///
    /// is equivalent to
    ///
    /// ```lua
    /// some.thing.methodname = function(self, ...) ... end
    /// ```
    SMethod(Variable<'a>, Spanned<&'a str>, Function<'a>),

    /// Assign function to newly declared local.
    ///
    /// ```lua
    /// local function XY(...) ... end
    /// ```
    ///
    /// is equivalent to
    ///
    /// ```lua
    /// local XY; XY = function(...) ... end
    /// ```
    SLFunc(Spanned<&'a str>, Function<'a>),

    /// Executes `body` if `cond` is true and the `elseif`s or `else` if not.
    SIf {
        cond: Expr<'a>,
        body: Block<'a>,
        elifs: Vec<Spanned<(/* cond */ Expr<'a>, /* body */ Block<'a>)>>,
        el: Option<Block<'a>>,
    },

    /// Loops a block while `cond` is true
    SWhile {
        cond: Expr<'a>,
        body: Block<'a>,
    },

    /// Loops a block until `abort_on` is true. The body is executed at least once (`abort_on` is
    /// checked after the body has run).
    SRepeat {
        abort_on: Expr<'a>,
        body: Block<'a>,
    },

    /// Numeric for loop
    SFor {
        var: Spanned<&'a str>,    // named local, newly declared
        start: Expr<'a>,
        step: Option<Expr<'a>>,
        end: Expr<'a>,
        body: Block<'a>,
    },

    /// Generic for loop
    SForIn {
        /// The loop variables, returned by iterator
        vars: Vec<Spanned<&'a str>>,
        /// Expression list: Iterator function, state, start value, [ignored ...]
        iter: Vec<Expr<'a>>,
        body: Block<'a>,
    },
}

#[derive(Clone, PartialEq, Debug)]
pub enum TableEntry<'a> {
    /// A key-value-pair
    Pair(Expr<'a>, Expr<'a>),
    /// An element of the table's array part
    Elem(Expr<'a>),
}

pub type TableCons<'a> = Vec<TableEntry<'a>>;

/// Expression nodes
#[derive(Clone, PartialEq, Debug)]
pub enum _Expr<'a> {
    ELit(Literal),
    EBinOp(Box<Expr<'a>>, BinOp, Box<Expr<'a>>),
    EUnOp(UnOp, Box<Expr<'a>>),
    EBraced(Box<Expr<'a>>),

    /// Variable used as expression
    EVar(Variable<'a>),
    /// Calls a function, might return multiple results
    ECall(Call<'a>),

    /// Instantiates a function/closure
    EFunc(Function<'a>),

    /// Table constructor
    ETable(TableCons<'a>),
    /// Array constructor, takes a list of initial values
    EArray(Vec<Expr<'a>>),
    /// "..."; expands to var args. only valid if used inside varargs functions
    EVarArgs,
}

pub type Expr<'a> = Spanned<_Expr<'a>>;
pub type Stmt<'a> = Spanned<_Stmt<'a>>;
pub type Variable<'a> = Spanned<_Variable<'a>>;
