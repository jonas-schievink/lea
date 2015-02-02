pub use self::Variable::*;
pub use self::Stmt::*;
pub use self::Expr::*;
pub use self::Node::*;

use self::Literal::*;
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

#[derive(Clone, PartialEq, Debug)]
pub struct Local {
    name: String,
}

/// A block containing any number of statements. All blocks carry a scope in which local variables
/// can be declared.
#[derive(Clone, Debug)]
pub struct Block {
    stmts: Vec<Stmt>,
    locals: Vec<Local>,
}

/// Something that can be assigned to a value
#[derive(Clone, PartialEq, Debug)]
pub enum Variable {
    /// References a named variable; later resolved to local or global
    VNamed(String),

    /// References the local variable in the current scope with the given name
    VLocal(String),

    /// References a named global
    VGlobal(String),

    /// References an indexed variable (a field)
    VIndex(Box<Variable>, String),
}

/// Statement nodes
#[derive(Clone, Debug)]
pub enum Stmt {
    /// Declare a list of locals and assign initial values.
    ///
    /// Initial values are optional and default to `nil`.
    SDecl(Vec<String>, Vec<Expr>),

    /// Assigns a list of expressions to a list of variables.
    ///
    /// Conatins at least one pair. If the last expression is a function or varargs, all return
    /// values are considered for assignment to the leftover variables.
    SAssign(Vec<Variable>, Vec<Expr>),

    SIf {
        cond: Expr,
        thenblock: Block,
        elseblock: Block,
    },

    /// Numeric for loop
    SFor {
        var: Local,
        start: Expr,
        step: Expr,
        end: Expr,
        body: Block,
    },

    /// Generic for loop
    SForIn {
        /// The loop variables, returned by iterator
        vars: Vec<Local>,
        /// Expression list: Iterator function, invariant state, start value, [ignored]
        iter: Vec<Expr>,
        body: Block,
    }
}

/// Expression nodes
#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    ELit(Literal),
    EBinOp(Box<Expr>, BinOp, Box<Expr>),
    EUnOp(UnOp, Box<Expr>),

    EVar(Variable),
}

/// AST nodes
#[derive(Clone)]
pub enum Node {
    ExprNode(Expr),
    StmtNode(Stmt),
}
