//! AST visitor implementation

use super::*;

use std::mem;

// FIXME Remove this workaround
trait MapInPlace {
    type Elem: Sized;
    fn map_in_place<F>(self, f: F) -> Self where F: FnMut(Self::Elem) -> Self::Elem;
}
impl<T> MapInPlace for Vec<T> {
    type Elem = T;
    fn map_in_place<F>(self, f: F) -> Self where F: FnMut(Self::Elem) -> Self::Elem {
        self.into_iter().map(f).collect::<Vec<T>>()
    }
}


/// A visitor that can transform AST nodes
pub trait Transform<'a> : Sized {
    fn visit_stmt(&mut self, stmt: Stmt<'a>) -> Stmt<'a> {
        walk_stmt(stmt, self)
    }
    fn visit_expr(&mut self, expr: Expr<'a>) -> Expr<'a> {
        walk_expr(expr, self)
    }
    fn visit_var(&mut self, var: Variable<'a>) -> Variable<'a> {
        walk_var(var, self)
    }
    fn visit_block(&mut self, block: Block<'a>) -> Block<'a> {
        walk_block(block, self)
    }
    fn visit_func(&mut self, mut func: Function<'a>) -> Function<'a> {
        func.body = self.visit_block(func.body);
        func
    }
}

/// A "read-only" visitor that gets a shared reference to the AST node
pub trait Visitor<'a> : Sized {
    fn visit_stmt(&mut self, stmt: &'a Stmt<'a>) {
        walk_stmt_ref(stmt, self);
    }
    fn visit_expr(&mut self, expr: &'a Expr<'a>) {
        walk_expr_ref(expr, self);
    }
    fn visit_var(&mut self, var: &'a Variable<'a>) {
        walk_var_ref(var, self);
    }
    fn visit_block(&mut self, block: &'a Block<'a>) {
        walk_block_ref(block, self);
    }
    fn visit_func(&mut self, func: &'a Function<'a>) {
        self.visit_block(&func.body);
    }
}


pub fn walk_block<'a, V: Transform<'a>>(mut b: Block<'a>, visitor: &mut V) -> Block<'a> {
    b.stmts = b.stmts.map_in_place(|stmt| visitor.visit_stmt(stmt));

    b
}

pub fn walk_block_ref<'a, V: Visitor<'a>>(b: &'a Block, visitor: &mut V) {
    for stmt in &b.stmts {
        visitor.visit_stmt(stmt);
    }
}

/// Helper function that walks the function's body.
pub fn walk_func<'a, V: Transform<'a>>(mut f: Function<'a>, visitor: &mut V) -> Function<'a> {
    // this is needed since rustc is too strict when doing something like:
    // func.body = walk_block(func.body, visitor);
    let mut body = mem::replace(&mut f.body, Block::new(vec![], Default::default()));
    body = walk_block(body, visitor);
    mem::replace(&mut f.body, body);
    f
}

pub fn walk_stmt<'a, V: Transform<'a>>(mut stmt: Stmt<'a>, visitor: &mut V) -> Stmt<'a> {
    stmt.value = match stmt.value {
        StmtKind::Decl(names, mut vals) => {
            vals = vals.map_in_place(|val| visitor.visit_expr(val));
            StmtKind::Decl(names, vals)
        },
        StmtKind::Assign(mut vars, mut vals) => {
            vars = vars.map_in_place(|var| visitor.visit_var(var));
            vals = vals.map_in_place(|val| visitor.visit_expr(val));
            StmtKind::Assign(vars, vals)
        },
        StmtKind::Do(mut block) => {
            block = visitor.visit_block(block);
            StmtKind::Do(block)
        },
        StmtKind::Return(mut vals) => {
            vals = vals.map_in_place(|val| visitor.visit_expr(val));
            StmtKind::Return(vals)
        },
        StmtKind::Call(Call::Normal(mut callee, mut argv)) => {
            callee = Box::new(visitor.visit_expr(*callee));
            argv = argv.map_in_place(|arg| visitor.visit_expr(arg));
            StmtKind::Call(Call::Normal(callee, argv))
        },
        StmtKind::Call(Call::Method(mut callee, name, mut argv)) => {
            callee = Box::new(visitor.visit_expr(*callee));
            argv = argv.map_in_place(|arg| visitor.visit_expr(arg));
            StmtKind::Call(Call::Method(callee, name, argv))
        },
        StmtKind::If {mut cond, mut body, mut el} => {
            cond = visitor.visit_expr(cond);
            body = visitor.visit_block(body);
            el = el.map(|el| visitor.visit_block(el));
            StmtKind::If {cond: cond, body: body, el: el}
        },
        StmtKind::While {mut cond, mut body} => {
            cond = visitor.visit_expr(cond);
            body = visitor.visit_block(body);
            StmtKind::While {cond: cond, body: body}
        },
        StmtKind::Repeat {mut abort_on, mut body} => {
            abort_on = visitor.visit_expr(abort_on);
            body = visitor.visit_block(body);
            StmtKind::Repeat {abort_on: abort_on, body: body}
        },
        StmtKind::For {mut start, mut step, mut end, mut body, var} => {
            start = visitor.visit_expr(start);
            step = step.map(|s| visitor.visit_expr(s));
            end = visitor.visit_expr(end);
            body = visitor.visit_block(body);
            StmtKind::For {start: start, step: step, end: end, body: body, var: var}
        },
        StmtKind::ForIn {iter: mut iter_exprs, mut body, vars} => {
            iter_exprs = iter_exprs.map_in_place(|e| visitor.visit_expr(e));
            body = visitor.visit_block(body);
            StmtKind::ForIn {iter: iter_exprs, body: body, vars: vars}
        },

        StmtKind::Break => StmtKind::Break,
    };

    stmt
}


pub fn walk_stmt_ref<'a, V: Visitor<'a>>(stmt: &'a Stmt, visitor: &mut V) {
    match stmt.value {
        StmtKind::Decl(_, ref vals) => {
            for e in vals {
                visitor.visit_expr(e);
            }
        },
        StmtKind::Assign(ref vars, ref vals) => {
            for var in vars {
                visitor.visit_var(var);
            }

            for val in vals {
                visitor.visit_expr(val);
            }
        },
        StmtKind::Do(ref block) => {
            visitor.visit_block(block);
        },
        StmtKind::Return(ref vals) => {
            for val in vals {
                visitor.visit_expr(val);
            }
        },
        StmtKind::Call(Call::Normal(ref callee, ref argv)) => {
            visitor.visit_expr(&**callee);
            for arg in argv {
                visitor.visit_expr(arg);
            }
        },
        StmtKind::Call(Call::Method(ref callee, _, ref argv)) => {
            visitor.visit_expr(&**callee);
            for arg in argv {
                visitor.visit_expr(arg);
            }
        },
        StmtKind::If {ref cond, ref body, ref el} => {
            visitor.visit_expr(cond);
            visitor.visit_block(body);
            el.as_ref().map(|el| visitor.visit_block(el));
        },
        StmtKind::While {ref cond, ref body} => {
            visitor.visit_expr(cond);
            visitor.visit_block(body);
        },
        StmtKind::Repeat {ref abort_on, ref body} => {
            visitor.visit_expr(abort_on);
            visitor.visit_block(body);
        },
        StmtKind::For {ref start, ref step, ref end, ref body, ..} => {
            visitor.visit_expr(start);
            if let Some(ref stepexpr) = *step { visitor.visit_expr(stepexpr); }
            visitor.visit_expr(end);
            visitor.visit_block(body);
        },
        StmtKind::ForIn {iter: ref iter_exprs, ref body, ..} => {
            for e in iter_exprs {
                visitor.visit_expr(e);
            }
            visitor.visit_block(body);
        },

        StmtKind::Break => {},
    };
}

pub fn walk_expr<'a, V: Transform<'a>>(mut expr: Expr<'a>, visitor: &mut V) -> Expr<'a> {
    expr.value = match expr.value {
        ExprKind::BinOp(mut lhs, op, mut rhs) => {
            lhs = Box::new(visitor.visit_expr(*lhs));
            rhs = Box::new(visitor.visit_expr(*rhs));
            ExprKind::BinOp(lhs, op, rhs)
        },
        ExprKind::UnOp(op, mut operand) => {
            operand = Box::new(visitor.visit_expr(*operand));
            ExprKind::UnOp(op, operand)
        },
        ExprKind::Var(var) => {
            ExprKind::Var(visitor.visit_var(var))
        },
        ExprKind::Call(Call::Normal(mut callee, mut argv)) => {
            callee = Box::new(visitor.visit_expr(*callee));
            argv = argv.map_in_place(|arg| visitor.visit_expr(arg));
            ExprKind::Call(Call::Normal(callee, argv))
        },
        ExprKind::Call(Call::Method(mut callee, name, mut argv)) => {
            callee = Box::new(visitor.visit_expr(*callee));
            argv = argv.map_in_place(|arg| visitor.visit_expr(arg));
            ExprKind::Call(Call::Method(callee, name, argv))
        },
        ExprKind::Func(func) => {
            ExprKind::Func(visitor.visit_func(func))
        },
        ExprKind::Table(cons) => {
            ExprKind::Table(cons.map_in_place(|(key, value)| (visitor.visit_expr(key), visitor.visit_expr(value))))
        },
        ExprKind::Array(exprs) => {
            ExprKind::Array(exprs.map_in_place(|e| visitor.visit_expr(e)))
        },

        // Explicitly ignore these, they carry nothing visitable
        ExprKind::VarArgs => ExprKind::VarArgs,
        ExprKind::Lit(lit) => ExprKind::Lit(lit),
    };

    expr
}

pub fn walk_expr_ref<'a, V: Visitor<'a>>(expr: &'a Expr, visitor: &mut V) {
    match expr.value {
        ExprKind::BinOp(ref lhs, _, ref rhs) => {
            visitor.visit_expr(&**lhs);
            visitor.visit_expr(&**rhs);
        },
        ExprKind::UnOp(_, ref operand) => {
            visitor.visit_expr(&**operand);
        },
        ExprKind::Var(ref var) => {
            visitor.visit_var(var);
        },
        ExprKind::Call(Call::Normal(ref callee, ref argv)) => {
            visitor.visit_expr(&**callee);
            for arg in argv {
                visitor.visit_expr(arg);
            }
        },
        ExprKind::Call(Call::Method(ref callee, _, ref argv)) => {
            visitor.visit_expr(&**callee);
            for arg in argv {
                visitor.visit_expr(arg);
            }
        },
        ExprKind::Func(ref func) => {
            visitor.visit_func(func);
        },
        ExprKind::Table(ref cons) => {
            for &(ref k, ref v) in cons {
                visitor.visit_expr(k);
                visitor.visit_expr(v);
            }
        },
        ExprKind::Array(ref exprs) => {
            for e in exprs {
                visitor.visit_expr(e);
            }
        },

        // Explicitly ignore these, they carry nothing visitable
        ExprKind::VarArgs => {},
        ExprKind::Lit(_) => {},
    };
}

pub fn walk_var<'a, V: Transform<'a>>(mut var: Variable<'a>, visitor: &mut V) -> Variable<'a> {
    var.value = match var.value {
        VarKind::Indexed(mut var, mut idx) => {
            var = Box::new(visitor.visit_var(*var));
            idx = Box::new(visitor.visit_expr(*idx));
            VarKind::Indexed(var, idx)
        },
        VarKind::Named(name) => VarKind::Named(name),
        VarKind::Local(id) => VarKind::Local(id),
        VarKind::Upval(id) => VarKind::Upval(id),
    };

    var
}

pub fn walk_var_ref<'a, V: Visitor<'a>>(var: &'a Variable, visitor: &mut V) {
    match var.value {
        VarKind::Indexed(ref var, ref idx) => {
            visitor.visit_var(&**var);
            visitor.visit_expr(&**idx);
        },
        VarKind::Named(_) | VarKind::Local(_) | VarKind::Upval(_) => {},
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use ast::*;
    use parser::span::{Span, Spanned};

    use lea_core::Const;

    #[test]
    fn visit_count() {
        struct NoopVisitor {
            stmts: u8,
            exprs: u8,
            vars: u8,
        };
        impl<'a> Visitor<'a> for NoopVisitor {
            fn visit_stmt(&mut self, stmt: &Stmt) {
                self.stmts += 1;
                walk_stmt_ref(stmt, self);
            }
            fn visit_expr(&mut self, expr: &Expr) {
                self.exprs += 1;
                walk_expr_ref(expr, self);
            }
            fn visit_var(&mut self, var: &Variable) {
                self.vars += 1;
                walk_var_ref(var, self);
            }
        }

        let myblock = Block::new(vec![
            Spanned::default(StmtKind::Assign(
                vec![Spanned::default(VarKind::Named("i"))],
                vec![Spanned::default(ExprKind::Lit(Const::Number(0.into())))],
            )),
        ], Span::new(0, 0));
        let mut v = NoopVisitor {stmts: 0, exprs: 0, vars: 0};
        walk_block_ref(&myblock, &mut v);

        assert_eq!(v.stmts, 1);
        assert_eq!(v.exprs, 1);
        assert_eq!(v.vars, 1);
    }

    #[test]
    fn visit_mut() {
        struct MutVisitor;
        impl<'a> Transform<'a> for MutVisitor {
            fn visit_expr(&mut self, mut expr: Expr<'a>) -> Expr<'a> {
                expr.value = match expr.value {
                    ExprKind::Lit(Const::Bool(true)) => ExprKind::Lit(Const::Bool(false)),
                    _ => { return expr; }
                };

                expr
            }
        }

        let mut b = Block::new(vec![
            Spanned::default(StmtKind::Return(vec![Spanned::default(ExprKind::Lit(Const::Bool(true)))])),
        ], Span::new(0, 0));
        b = walk_block(b, &mut MutVisitor);

        assert_eq!(b.stmts, vec![
            Spanned::default(StmtKind::Return(vec![
                Spanned::default(ExprKind::Lit(Const::Bool(false)))
            ])),
        ]);
    }
}
