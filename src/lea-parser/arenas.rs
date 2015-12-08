//! Defines the arenas used to store the parse tree

// TODO: Implement the placer traits (just to see how it would feel)

use parsetree::{Expr, Stmt, Variable};

use typed_arena::Arena;

/// Arenas used to store a parse tree
pub struct ParseTreeArenas<'a> {
    expr: Arena<Expr<'a>>,
    stmt: Arena<Stmt<'a>>,
    vars: Arena<Variable<'a>>,
}

impl<'a> ParseTreeArenas<'a> {
    pub fn new() -> Self {
        ParseTreeArenas {
            expr: Arena::with_capacity(32),
            stmt: Arena::with_capacity(8),
            vars: Arena::with_capacity(4),
        }
    }

    /// Allocates an expression in the arena and return a mutable reference to it.
    pub fn alloc_expr(&'a self, expr: Expr<'a>) -> &mut Expr {
        self.expr.alloc(expr)
    }

    /// Allocates a statement in the arena and return a mutable reference to it.
    pub fn alloc_stmt(&'a self, stmt: Stmt<'a>) -> &mut Stmt {
        self.stmt.alloc(stmt)
    }

    /// Allocates a variable in the arena and return a mutable reference to it.
    pub fn alloc_var(&'a self, var: Variable<'a>) -> &mut Variable {
        self.vars.alloc(var)
    }
}
