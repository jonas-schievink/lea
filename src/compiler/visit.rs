//! AST visitor implementation

use super::ast::*;

use std::default::Default;
use std::mem;


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

fn walk_args<'a, V: Transform<'a>>(args: CallArgs<'a>, visitor: &mut V) -> CallArgs<'a> {
    match args {
        CallArgs::Normal(mut argv) => {
            argv = argv.map_in_place(|arg| visitor.visit_expr(arg));
            CallArgs::Normal(argv)
        },
        CallArgs::String(s) => {
            CallArgs::String(s)
        },
        CallArgs::Table(cons) => {
            CallArgs::Table(walk_table(cons, visitor))
        }
    }
}

fn walk_args_ref<'a, V: Visitor<'a>>(args: &'a CallArgs, visitor: &mut V) {
    match *args {
        CallArgs::Normal(ref argv) => {
            for arg in argv {
                visitor.visit_expr(arg);
            }
        },
        CallArgs::String(_) => {},
        CallArgs::Table(ref cons) => {
            walk_table_ref(cons, visitor)
        }
    }
}

fn walk_table<'a, V: Transform<'a>>(cons: TableCons<'a>, visitor: &mut V) -> TableCons<'a> {
    cons.map_in_place(|entry| match entry {
        TableEntry::Pair(k, v) => TableEntry::Pair(visitor.visit_expr(k), visitor.visit_expr(v)),
        TableEntry::Elem(elem) => TableEntry::Elem(visitor.visit_expr(elem)),
    })
}

fn walk_table_ref<'a, V: Visitor<'a>>(cons: &'a TableCons, visitor: &mut V) {
    for entry in cons {
        match *entry {
            TableEntry::Pair(ref k, ref v) => {
                visitor.visit_expr(k);
                visitor.visit_expr(v);
            }
            TableEntry::Elem(ref elem) => {
                visitor.visit_expr(elem);
            }
        }
    }
}

pub fn walk_stmt<'a, V: Transform<'a>>(mut stmt: Stmt<'a>, visitor: &mut V) -> Stmt<'a> {
    stmt.value = match stmt.value {
        SDecl(names, mut vals) => {
            vals = vals.map_in_place(|val| visitor.visit_expr(val));
            SDecl(names, vals)
        },
        SAssign(mut vars, mut vals) => {
            vars = vars.map_in_place(|var| visitor.visit_var(var));
            vals = vals.map_in_place(|val| visitor.visit_expr(val));
            SAssign(vars, vals)
        },
        SDo(mut block) => {
            block = visitor.visit_block(block);
            SDo(block)
        },
        SReturn(mut vals) => {
            vals = vals.map_in_place(|val| visitor.visit_expr(val));
            SReturn(vals)
        },
        SCall(SimpleCall(mut callee, mut argv)) => {
            callee = Box::new(visitor.visit_expr(*callee));
            argv = walk_args(argv, visitor);
            SCall(SimpleCall(callee, argv))
        },
        SCall(MethodCall(mut callee, name, mut argv)) => {
            callee = Box::new(visitor.visit_expr(*callee));
            argv = walk_args(argv, visitor);
            SCall(MethodCall(callee, name, argv))
        },
        SFunc(mut var, mut func) => {
            var = visitor.visit_var(var);
            func = visitor.visit_func(func);
            SFunc(var, func)
        },
        SMethod(mut var, name, mut func) => {
            var = visitor.visit_var(var);
            func = visitor.visit_func(func);
            SMethod(var, name, func)
        },
        SLFunc(name, mut func) => {
            func = visitor.visit_func(func);
            SLFunc(name, func)
        },
        SIf {mut cond, mut body, mut el} => {
            cond = visitor.visit_expr(cond);
            body = visitor.visit_block(body);
            el = visitor.visit_block(el);
            SIf {cond: cond, body: body, el: el}
        },
        SWhile {mut cond, mut body} => {
            cond = visitor.visit_expr(cond);
            body = visitor.visit_block(body);
            SWhile {cond: cond, body: body}
        },
        SRepeat {mut abort_on, mut body} => {
            abort_on = visitor.visit_expr(abort_on);
            body = visitor.visit_block(body);
            SRepeat {abort_on: abort_on, body: body}
        },
        SFor {mut start, mut step, mut end, mut body, var} => {
            start = visitor.visit_expr(start);
            step = step.map(|s| visitor.visit_expr(s));
            end = visitor.visit_expr(end);
            body = visitor.visit_block(body);
            SFor {start: start, step: step, end: end, body: body, var: var}
        },
        SForIn {iter: mut iter_exprs, mut body, vars} => {
            iter_exprs = iter_exprs.map_in_place(|e| visitor.visit_expr(e));
            body = visitor.visit_block(body);
            SForIn {iter: iter_exprs, body: body, vars: vars}
        },

        SBreak => SBreak,
    };

    stmt
}


pub fn walk_stmt_ref<'a, V: Visitor<'a>>(stmt: &'a Stmt, visitor: &mut V) {
    match stmt.value {
        SDecl(_, ref vals) => {
            for e in vals {
                visitor.visit_expr(e);
            }
        },
        SAssign(ref vars, ref vals) => {
            for var in vars {
                visitor.visit_var(var);
            }

            for val in vals {
                visitor.visit_expr(val);
            }
        },
        SDo(ref block) => {
            visitor.visit_block(block);
        },
        SReturn(ref vals) => {
            for val in vals {
                visitor.visit_expr(val);
            }
        },
        SCall(SimpleCall(ref callee, ref argv)) => {
            visitor.visit_expr(&**callee);
            walk_args_ref(argv, visitor);
        },
        SCall(MethodCall(ref callee, _, ref argv)) => {
            visitor.visit_expr(&**callee);
            walk_args_ref(argv, visitor);
        },
        SFunc(ref var, ref func) => {
            visitor.visit_var(var);
            visitor.visit_func(func);
        },
        SMethod(ref var, _, ref func) => {
            visitor.visit_var(var);
            visitor.visit_func(func);
        },
        SLFunc(_, ref func) => {
            visitor.visit_func(func);
        },
        SIf {ref cond, ref body, ref el} => {
            visitor.visit_expr(cond);
            visitor.visit_block(body);
            visitor.visit_block(el);
        },
        SWhile {ref cond, ref body} => {
            visitor.visit_expr(cond);
            visitor.visit_block(body);
        },
        SRepeat {ref abort_on, ref body} => {
            visitor.visit_expr(abort_on);
            visitor.visit_block(body);
        },
        SFor {ref start, ref step, ref end, ref body, ..} => {
            visitor.visit_expr(start);
            if let Some(ref stepexpr) = *step { visitor.visit_expr(stepexpr); }
            visitor.visit_expr(end);
            visitor.visit_block(body);
        },
        SForIn {iter: ref iter_exprs, ref body, ..} => {
            for e in iter_exprs {
                visitor.visit_expr(e);
            }
            visitor.visit_block(body);
        },

        SBreak => {},
    };
}

pub fn walk_expr<'a, V: Transform<'a>>(mut expr: Expr<'a>, visitor: &mut V) -> Expr<'a> {
    expr.value = match expr.value {
        ERawOp(mut lhs, mut rest) => {
            lhs = Box::new(visitor.visit_expr(*lhs));
            rest = rest.map_in_place(|(a, expr)| (a, visitor.visit_expr(expr)));
            ERawOp(lhs, rest)
        },
        EBinOp(mut lhs, op, mut rhs) => {
            lhs = Box::new(visitor.visit_expr(*lhs));
            rhs = Box::new(visitor.visit_expr(*rhs));
            EBinOp(lhs, op, rhs)
        },
        EBraced(mut e) => {
            e = Box::new(visitor.visit_expr(*e));
            EBraced(e)
        }
        EUnOp(op, mut operand) => {
            operand = Box::new(visitor.visit_expr(*operand));
            EUnOp(op, operand)
        },
        EVar(var) => {
            EVar(visitor.visit_var(var))
        },
        ECall(SimpleCall(mut callee, mut argv)) => {
            callee = Box::new(visitor.visit_expr(*callee));
            argv = walk_args(argv, visitor);
            ECall(SimpleCall(callee, argv))
        },
        ECall(MethodCall(mut callee, name, mut argv)) => {
            callee = Box::new(visitor.visit_expr(*callee));
            argv = walk_args(argv, visitor);
            ECall(MethodCall(callee, name, argv))
        },
        EFunc(func) => {
            EFunc(visitor.visit_func(func))
        },
        ETable(cons) => {
            ETable(walk_table(cons, visitor))
        },
        EArray(exprs) => {
            EArray(exprs.map_in_place(|e| visitor.visit_expr(e)))
        },

        // Explicitly ignore these, they carry nothing visitable
        EVarArgs => EVarArgs,
        ELit(lit) => ELit(lit),
    };

    expr
}

pub fn walk_expr_ref<'a, V: Visitor<'a>>(expr: &'a Expr, visitor: &mut V) {
    match expr.value {
        ERawOp(ref lhs, ref rest) => {
            visitor.visit_expr(&**lhs);
            for entry in rest {
                let (_, ref e) = *entry;
                visitor.visit_expr(e);
            }
        },
        EBinOp(ref lhs, _, ref rhs) => {
            visitor.visit_expr(&**lhs);
            visitor.visit_expr(&**rhs);
        },
        EUnOp(_, ref operand) => {
            visitor.visit_expr(&**operand);
        },
        EBraced(ref e) => {
            visitor.visit_expr(&**e);
        },
        EVar(ref var) => {
            visitor.visit_var(var);
        },
        ECall(SimpleCall(ref callee, ref argv)) => {
            visitor.visit_expr(&**callee);
            walk_args_ref(argv, visitor);
        },
        ECall(MethodCall(ref callee, _, ref argv)) => {
            visitor.visit_expr(&**callee);
            walk_args_ref(argv, visitor);
        },
        EFunc(ref func) => {
            visitor.visit_func(func);
        },
        ETable(ref cons) => {
            walk_table_ref(cons, visitor);
        },
        EArray(ref exprs) => {
            for e in exprs {
                visitor.visit_expr(e);
            }
        },

        // Explicitly ignore these, they carry nothing visitable
        EVarArgs => {},
        ELit(_) => {},
    };
}

pub fn walk_var<'a, V: Transform<'a>>(mut var: Variable<'a>, visitor: &mut V) -> Variable<'a> {
    var.value = match var.value {
        VIndex(mut var, mut idx) => {
            var = Box::new(visitor.visit_var(*var));
            idx = Box::new(visitor.visit_expr(*idx));
            VIndex(var, idx)
        },
        VDotIndex(mut var, strn) => {
            var = Box::new(visitor.visit_var(*var));
            VDotIndex(var, strn)
        },
        VResGlobal(mut env, name) => {
            env = Box::new(visitor.visit_var(*env));
            VResGlobal(env, name)
        }
        VNamed(name) => VNamed(name),
        VLocal(id) => VLocal(id),
        VUpval(id) => VUpval(id),
    };

    var
}

pub fn walk_var_ref<'a, V: Visitor<'a>>(var: &'a Variable, visitor: &mut V) {
    match var.value {
        VIndex(ref var, ref idx) => {
            visitor.visit_var(&**var);
            visitor.visit_expr(&**idx);
        },
        VDotIndex(ref var, _) => {
            visitor.visit_var(&**var);
        },
        VResGlobal(ref env, _) => {
            visitor.visit_var(&**env);
        }
        VNamed(_) | VLocal(_) | VUpval(_) => {},
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;
    use parser::block;
    use span::Spanned;

    use core::literal::*;

    use std::default::Default;

    #[test]
    fn visit_noop() {
        struct NoopVisitor {
            stmts: u8,
            exprs: u8,
            vars: u8,
        };
        impl <'a> Visitor<'a> for NoopVisitor {
            fn visit_stmt(&mut self, _stmt: &Stmt) {
                self.stmts += 1;
            }
            fn visit_expr(&mut self, _expr: &Expr) {
                self.exprs += 1;
            }
            fn visit_var(&mut self, _var: &Variable) {
                self.vars += 1;
            }
        }

        let myblock = block("i = 0").unwrap();
        let mut v = NoopVisitor {stmts: 0, exprs: 0, vars: 0};
        walk_block_ref(&myblock, &mut v);

        // The statement is always visited, the expr and var are skipped because we don't call
        // the walk_* functions.
        assert_eq!(v.stmts, 1);
        assert_eq!(v.exprs, 0);
        assert_eq!(v.vars, 0);
    }

    #[test]
    fn visit_vars() {
        struct VarVisitor<'a> {
            vars: Vec<Variable<'a>>,
        }
        impl <'a> Visitor<'a> for VarVisitor<'a> {
            fn visit_stmt(&mut self, stmt: &'a Stmt<'a>) {
                walk_stmt_ref(stmt, self);
            }
            fn visit_expr(&mut self, expr: &'a Expr<'a>) {
                walk_expr_ref(expr, self);
            }
            fn visit_var(&mut self, var: &'a Variable<'a>) {
                self.vars.push(var.clone());
                walk_var_ref(var, self);
            }
        }

        let myblock = block("local i = a\nj = i").unwrap();
        let mut v = VarVisitor { vars: Vec::new() };
        walk_block_ref(&myblock, &mut v);

        // The first "i" is not visited, since it's stored as a string. Everything else is a
        // "VNamed" since variable resolution hasn't yet taken place.
        assert_eq!(v.vars, vec![
            Spanned::default(VNamed("a")),
            Spanned::default(VNamed("j")),
            Spanned::default(VNamed("i")),
        ]);
    }

    #[test]
    fn visit_mut() {
        struct MutVisitor;
        impl <'a> Transform<'a> for MutVisitor {
            fn visit_expr(&mut self, mut expr: Expr<'a>) -> Expr<'a> {
                expr.value = match expr.value {
                    ELit(TInt(1)) => ELit(TInt(0)),
                    _ => { return expr; }
                };

                expr
            }
        }

        let mut b = block("return 1").unwrap();
        b = walk_block(b, &mut MutVisitor);

        assert_eq!(b.stmts, vec![
            Spanned::default(SReturn(vec![
                Spanned::default(ELit(TInt(0)))
            ])),
        ]);
    }
}
