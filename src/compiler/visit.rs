//! AST visitor implementation

use super::ast::*;

use std::default::Default;
use std::mem;

pub trait Visitor : Sized {
    fn visit_stmt(&mut self, stmt: Stmt) -> Stmt {
        walk_stmt(stmt, self)
    }
    fn visit_expr(&mut self, expr: Expr) -> Expr {
        walk_expr(expr, self)
    }
    fn visit_var(&mut self, var: Variable) -> Variable {
        walk_var(var, self)
    }
    fn visit_block(&mut self, block: Block) -> Block {
        walk_block(block, self)
    }
    fn visit_func(&mut self, mut func: Function) -> Function {
        func.value.body = self.visit_block(func.value.body);
        func
    }
}


pub fn walk_block<V: Visitor>(mut b: Block, visitor: &mut V) -> Block {
    b.stmts = b.stmts.map_in_place(|stmt| visitor.visit_stmt(stmt));

    b
}

/// Helper function that walks the function's body.
pub fn walk_func<V: Visitor>(mut f: Function, visitor: &mut V) -> Function {
    // this is needed since rustc is too strict when doing something like:
    // func.body = walk_block(func.body, visitor);
    let mut body = mem::replace(&mut f.body, Block::new(vec![], Default::default()));
    body = walk_block(body, visitor);
    mem::replace(&mut f.body, body);
    f
}

pub fn walk_stmt<V: Visitor>(mut stmt: Stmt, visitor: &mut V) -> Stmt {
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
        SCall(Call{mut callee, mut argv}) => {
            callee = Box::new(visitor.visit_expr(*callee));
            argv = argv.map_in_place(|arg| visitor.visit_expr(arg));
            SCall(Call{callee: callee, argv: argv})
        },
        SFunc(mut var, mut func) => {
            var = visitor.visit_var(var);
            func = visitor.visit_func(func);
            SFunc(var, func)
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
            step = visitor.visit_expr(step);
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

pub fn walk_expr<V: Visitor>(mut expr: Expr, visitor: &mut V) -> Expr {
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
        EUnOp(op, mut operand) => {
            operand = Box::new(visitor.visit_expr(*operand));
            EUnOp(op, operand)
        },
        EVar(mut var) => {
            var = visitor.visit_var(var);
            EVar(var)
        },
        ECall(Call{mut callee, mut argv}) => {
            callee = Box::new(visitor.visit_expr(*callee));
            argv = argv.map_in_place(|arg| visitor.visit_expr(arg));
            ECall(Call {callee: callee, argv: argv})
        },
        EFunc(mut func) => {
            func = visitor.visit_func(func);
            EFunc(func)
        },
        ETable(mut pairs) => {
            pairs = pairs.map_in_place(
                |(key, val)| (visitor.visit_expr(key), visitor.visit_expr(val)));
            ETable(pairs)
        },
        EArray(mut exprs) => {
            exprs = exprs.map_in_place(|e| visitor.visit_expr(e));
            EArray(exprs)
        },

        // Explicitly ignore these, they carry nothing visitable
        EVarArgs => EVarArgs,
        ELit(lit) => ELit(lit),
    };

    expr
}

pub fn walk_var<V: Visitor>(mut var: Variable, visitor: &mut V) -> Variable {
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
        VGlobal(name) => VGlobal(name),
    };

    var
}

#[cfg(test)]
mod tests {
    use super::*;
    use compiler::ast::*;
    use compiler::parser::block;
    use compiler::span::Spanned;

    use std::default::Default;

    #[test]
    fn visit_noop() {
        struct NoopVisitor {
            stmts: u8,
            exprs: u8,
            vars: u8,
        };
        impl Visitor for NoopVisitor {
            fn visit_stmt(&mut self, stmt: Stmt) -> Stmt {
                self.stmts += 1;
                stmt
            }
            fn visit_expr(&mut self, expr: Expr) -> Expr {
                self.exprs += 1;
                expr
            }
            fn visit_var(&mut self, var: Variable) -> Variable {
                self.vars += 1;
                var
            }
        }

        let myblock = block("i = 0").unwrap();
        let mut v = NoopVisitor {stmts: 0, exprs: 0, vars: 0};
        walk_block(myblock, &mut v);

        // The statement is always visited, the expr and var are skipped because we don't call
        // the walk_* functions.
        assert_eq!(v.stmts, 1);
        assert_eq!(v.exprs, 0);
        assert_eq!(v.vars, 0);
    }

    #[test]
    fn visit_vars() {
        struct VarVisitor {
            vars: Vec<Variable>,
        }
        impl Visitor for VarVisitor {
            fn visit_stmt(&mut self, stmt: Stmt) -> Stmt {
                walk_stmt(stmt, self)
            }
            fn visit_expr(&mut self, expr: Expr) -> Expr {
                walk_expr(expr, self)
            }
            fn visit_var(&mut self, var: Variable) -> Variable {
                self.vars.push(var.clone());
                walk_var(var, self)
            }
        }

        let myblock = block("local i = a\nj = i").unwrap();
        let mut v = VarVisitor { vars: Vec::new() };
        walk_block(myblock, &mut v);

        // The first "i" is not visited, since it's stored as a string. Everything else is a
        // "VNamed" since variable resolution hasn't yet taken place.
        assert_eq!(v.vars, vec![
            Spanned::default(VNamed("a".to_string())),
            Spanned::default(VNamed("j".to_string())),
            Spanned::default(VNamed("i".to_string())),
        ]);
    }

    #[test]
    fn visit_mut() {
        struct MutVisitor;
        impl Visitor for MutVisitor {
            fn visit_expr(&mut self, mut expr: Expr) -> Expr {
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
