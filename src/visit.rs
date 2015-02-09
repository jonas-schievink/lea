//! AST visitor implementation

use ast::*;

pub trait Visitor : Sized {
    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        walk_stmt(stmt, self);
    }
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(expr, self);
    }
    fn visit_var(&mut self, var: &mut Variable) {
        walk_var(var, self);
    }
    fn visit_block(&mut self, block: &mut Block) {
        walk_block(block, self);
    }
    fn visit_func(&mut self, func: &mut Function) {
        self.visit_block(&mut func.value.body);
    }
}

pub fn walk_block<V: Visitor>(b: &mut Block, visitor: &mut V) {
    for stmt in &mut b.value.stmts {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_stmt<V: Visitor>(stmt: &mut Stmt, visitor: &mut V) {
    match stmt.value {
        SDecl(_, ref mut vals) => {
            for val in vals {
                visitor.visit_expr(val);
            }
        },
        SAssign(ref mut vars, ref mut vals) => {
            for var in vars {
                visitor.visit_var(var);
            }
            for val in vals {
                visitor.visit_expr(val);
            }
        },
        SDo(ref mut block) => {
            visitor.visit_block(block);
        },
        SReturn(ref mut vals) => {
            for val in vals {
                visitor.visit_expr(val);
            }
        },
        SCall(Call{ref mut callee, ref mut argv}) => {
            visitor.visit_expr(callee);
            for arg in argv {
                visitor.visit_expr(arg);
            }
        },
        SFunc(ref mut var, ref mut func) => {
            visitor.visit_var(var);
            visitor.visit_func(func);
        },
        SLFunc(_, ref mut func) => {
            visitor.visit_func(func);
        },
        SIf {ref mut cond, ref mut body, ref mut el} => {
            visitor.visit_expr(cond);
            visitor.visit_block(body);
            visitor.visit_block(el);
        },
        SWhile {ref mut cond, ref mut body} => {
            visitor.visit_expr(cond);
            visitor.visit_block(body);
        },
        SRepeat {ref mut abort_on, ref mut body} => {
            visitor.visit_expr(abort_on);
            visitor.visit_block(body);
        },
        SFor {ref mut start, ref mut step, ref mut end, ref mut body, ..} => {
            visitor.visit_expr(start);
            visitor.visit_expr(step);
            visitor.visit_expr(end);
            visitor.visit_block(body);
        },
        SForIn {iter: ref mut iter_exprs, ref mut body, ..} => {
            for ref mut expr in iter_exprs {
                visitor.visit_expr(expr);
            }
            visitor.visit_block(body);
        },

        SBreak => {},
    }
}

pub fn walk_expr<V: Visitor>(expr: &mut Expr, visitor: &mut V) {
    match expr.value {
        ERawOp(ref mut lhs, ref mut rest) => {
            visitor.visit_expr(lhs);
            for t in rest {
                let (_, ref mut r) = *t;
                visitor.visit_expr(r);
            }
        },
        EBinOp(ref mut lhs, _op, ref mut rhs) => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        },
        EUnOp(_op, ref mut operand) => {
            visitor.visit_expr(operand);
        },
        EVar(ref mut var) => {
            visitor.visit_var(var);
        },
        ECall(Call{ref mut callee, ref mut argv}) => {
            visitor.visit_expr(callee);
            for arg in argv {
                visitor.visit_expr(arg);
            }
        },
        EFunc(ref mut func) => {
            visitor.visit_func(func);
        },
        ETable(ref mut pairs) => {
            for &mut (ref mut key, ref mut val) in pairs {
                visitor.visit_expr(key);
                visitor.visit_expr(val);
            }
        },
        EArray(ref mut exprs) => {
            for expr in exprs {
                visitor.visit_expr(expr);
            }
        },

        // Explicitly ignore these, they carry nothing visitable
        EVarArgs => {},
        ELit(..) => {},
    }
}

pub fn walk_var<V: Visitor>(var: &mut Variable, visitor: &mut V) {
    match var.value {
        VIndex(ref mut var, ref mut idx) => {
            visitor.visit_var(var);
            visitor.visit_expr(idx);
        },
        VDotIndex(ref mut var, _) => {
            visitor.visit_var(var);
        },

        VNamed(..) => {},
        VLocal(..) => {},
        VGlobal(..) => {},
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;
    use parser::block;
    use span::Spanned;

    #[test]
    fn visit_noop() {
        struct NoopVisitor {
            stmts: u8,
            exprs: u8,
            vars: u8,
        };
        impl Visitor for NoopVisitor {
            fn visit_stmt(&mut self, _stmt: &mut Stmt) {
                self.stmts += 1;
            }
            fn visit_expr(&mut self, _expr: &mut Expr) {
                self.exprs += 1;
            }
            fn visit_var(&mut self, _var: &mut Variable) {
                self.vars += 1;
            }
        }

        let mut myblock = block("i = 0").unwrap();
        let mut v = NoopVisitor {stmts: 0, exprs: 0, vars: 0};
        walk_block(&mut myblock, &mut v);

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
            fn visit_stmt(&mut self, stmt: &mut Stmt) {
                walk_stmt(stmt, self);
            }
            fn visit_expr(&mut self, expr: &mut Expr) {
                walk_expr(expr, self);
            }
            fn visit_var(&mut self, var: &mut Variable) {
                self.vars.push(var.clone());
                walk_var(var, self);
            }
        }

        let mut myblock = block("local i = a\nj = i").unwrap();
        let mut v = VarVisitor { vars: Vec::new() };
        walk_block(&mut myblock, &mut v);

        // The first "i" is not visited, since it's stored as a string. Everything else is a
        // "VNamed" since variable resolution hasn't yet taken place.
        assert_eq!(v.vars, vec![
            Spanned::default(VNamed("a".to_string())),
            Spanned::default(VNamed("j".to_string())),
            Spanned::default(VNamed("i".to_string())),
        ]);
    }
}
