//! Parse tree -> AST conversion

// TODO Do resolve and fold, possibly some lints in this pass

use super::*;
use parser::parsetree;
use parser::span::{Span, Spanned};

use lea_core::Const;

use std::vec;
use std::mem;


enum ConvResult<T> {
    Zero,
    One(T),
    Two(T, T),

    #[allow(dead_code)] // for future expansion
    Many(Vec<T>),
}

impl<T> ConvResult<T> {
    /// Spans all values with `span` and returns a new result that yields spanned values.
    fn spanned(self, span: Span) -> ConvResult<Spanned<T>> {
        match self {
            ConvResult::Zero => ConvResult::Zero,
            ConvResult::One(t) => ConvResult::One(Spanned::new(span, t)),
            ConvResult::Two(t, t2) => ConvResult::Two(Spanned::new(span, t), Spanned::new(span, t2)),
            ConvResult::Many(v) => ConvResult::Many(
                v.into_iter().map(|elem| Spanned::new(span, elem)).collect()
            ),
        }
    }
}

impl<T> IntoIterator for ConvResult<T> {
    type Item = T;
    type IntoIter = ConvResultIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            ConvResult::Zero => ConvResultIter::Zero,
            ConvResult::One(t) => ConvResultIter::One(t),
            ConvResult::Two(t, t2) => ConvResultIter::Two(t, t2),
            ConvResult::Many(v) => ConvResultIter::Many(v.into_iter()),
        }
    }
}

enum ConvResultIter<T> {
    Zero,
    One(T),
    Two(T, T),
    Many(vec::IntoIter<T>),
}

impl<T> Iterator for ConvResultIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let current = mem::replace(self, ConvResultIter::Zero);

        let (new, result) = match current {
            ConvResultIter::Zero => (ConvResultIter::Zero, None),
            ConvResultIter::One(t) => (ConvResultIter::Zero, Some(t)),
            ConvResultIter::Two(t, t2) => (ConvResultIter::One(t2), Some(t)),
            ConvResultIter::Many(mut v) => {
                let next = v.next();
                (ConvResultIter::Many(v), next)
            }
        };

        mem::replace(self, new);

        result
    }
}

struct AstConv;

impl AstConv {
    fn conv_func<'a>(&mut self, func: parsetree::Function<'a>) -> Function<'a> {
        Function {
            params: func.params,
            varargs: func.varargs,
            body: self.conv_block(func.body),
            locals: Default::default(),
            upvalues: Default::default(),
        }
    }

    fn conv_block<'a>(&mut self, block: parsetree::Block<'a>) -> Block<'a> {
        Block {
            span: block.span,
            stmts: block.stmts.into_iter().flat_map(|s| self.conv_stmt(s)).collect(),
            localmap: Default::default(),
        }
    }

    fn conv_stmt<'a>(&mut self, stmt: parsetree::Stmt<'a>) -> ConvResult<Stmt<'a>> {
        match stmt.value {
            parsetree::StmtKind::Decl(names, exprs) => {
                ConvResult::One(StmtKind::Decl(
                    names,
                    exprs.into_iter().map(|e| self.conv_expr(e)).collect()
                ))
            }
            parsetree::StmtKind::Assign(vars, exprs) => {
                ConvResult::One(StmtKind::Assign(
                    vars.into_iter().map(|var| self.conv_var(var)).collect(),
                    exprs.into_iter().map(|e| self.conv_expr(e)).collect()
                ))
            }
            parsetree::StmtKind::Do(block) => {
                ConvResult::One(StmtKind::Do(self.conv_block(block)))
            }
            parsetree::StmtKind::Break => {
                ConvResult::One(StmtKind::Break)
            }
            parsetree::StmtKind::Semi => {
                ConvResult::Zero
            }
            parsetree::StmtKind::Return(exprs) => {
                ConvResult::One(StmtKind::Return(
                    exprs.into_iter().map(|e| self.conv_expr(e)).collect()
                ))
            }
            parsetree::StmtKind::Call(call) => {
                ConvResult::One(StmtKind::Call(match call {
                    parsetree::Call::Normal(callee, args) => {
                        Call::Normal(Box::new(self.conv_expr(*callee)), self.conv_args(args))
                    }
                    parsetree::Call::Method(obj, name, args) => {
                        Call::Method(Box::new(self.conv_expr(*obj)), name, self.conv_args(args))
                    }
                }))
            }
            parsetree::StmtKind::Func(var, func) => {
                ConvResult::One(StmtKind::Assign(
                    vec![self.conv_var(var)],
                    vec![Spanned::new(func.body.span, ExprKind::Func(self.conv_func(func)))]
                ))
            }
            parsetree::StmtKind::Method(var, name, func) => {
                // function var:name(args...)   =>   var.name = function(self, args...)
                let mut func: Function<'a> = self.conv_func(func);
                func.params.insert(0, Spanned::new(name.span, "self"));

                ConvResult::One(StmtKind::Assign(
                    vec![Spanned::new(var.span, VarKind::Indexed(
                        Box::new(self.conv_var(var)),
                        Box::new(Spanned::new(name.span, ExprKind::Lit(Const::Str(name.value.to_owned()))))
                    ))],
                    vec![Spanned::new(func.body.span, ExprKind::Func(func))]
                ))
            }
            parsetree::StmtKind::LocalFunc(local, func) => {
                // local function f...  =>  local f; f = function...
                ConvResult::Two(
                    StmtKind::Decl(vec![local], vec![]),
                    StmtKind::Assign(
                        vec![Spanned::new(local.span, VarKind::Named(local.value))],
                        vec![Spanned::new(func.body.span, ExprKind::Func(self.conv_func(func)))]
                    )
                )
            }
            parsetree::StmtKind::If { cond, body, elifs, el } => {
                // Build else block for this if statement
                // Start with "pure" else at the end
                let mut myelse: Option<Block> = el.map(|el| self.conv_block(el));

                myelse = elifs.into_iter().fold(myelse, |old, elif| {
                    let (cond, body) = elif.value;

                    Some(Block {
                        localmap: Default::default(),
                        span: elif.span,
                        stmts: vec![
                            Spanned::new(elif.span, StmtKind::If {
                                cond: self.conv_expr(cond),
                                body: self.conv_block(body),
                                el: old,
                            }),
                        ],
                    })
                });

                ConvResult::One(StmtKind::If {
                    cond: self.conv_expr(cond),
                    body: self.conv_block(body),
                    el: myelse,
                })
            }
            parsetree::StmtKind::While { cond, body } => {
                ConvResult::One(StmtKind::While {
                    cond: self.conv_expr(cond),
                    body: self.conv_block(body),
                })
            }
            parsetree::StmtKind::Repeat { abort_on, body } => {
                ConvResult::One(StmtKind::Repeat {
                    abort_on: self.conv_expr(abort_on),
                    body: self.conv_block(body),
                })
            }
            parsetree::StmtKind::For { var, start, step, end, body } => {
                ConvResult::One(StmtKind::For {
                    var: var,
                    start: self.conv_expr(start),
                    step: step.map(|e| self.conv_expr(e)),
                    end: self.conv_expr(end),
                    body: self.conv_block(body),
                })
            }
            parsetree::StmtKind::ForIn { vars, iter, body } => {
                ConvResult::One(StmtKind::ForIn {
                    vars: vars,
                    iter: iter.into_iter().map(|e| self.conv_expr(e)).collect(),
                    body: self.conv_block(body),
                })
            }
        }.spanned(stmt.span)
    }

    fn conv_var<'a>(&mut self, var: parsetree::Variable<'a>) -> Variable<'a> {
        Spanned::new(var.span, match var.value {
            parsetree::VarKind::Named(name) => {
                VarKind::Named(name)
            }
            parsetree::VarKind::Indexed(var, index) => {
                VarKind::Indexed(Box::new(self.conv_var(*var)), match index {
                    parsetree::VarIndex::DotIndex(name) => {
                        Box::new(Spanned::new(name.span,
                            ExprKind::Lit(Const::Str(name.value.to_owned()))))
                    }
                    parsetree::VarIndex::ExprIndex(expr) => {
                        Box::new(self.conv_expr(*expr))
                    }
                })
            }
        })
    }

    fn conv_expr<'a>(&mut self, expr: parsetree::Expr<'a>) -> Expr<'a> {
        Spanned::new(expr.span, match expr.value {
            parsetree::ExprKind::Lit(lit) => ExprKind::Lit(lit),
            parsetree::ExprKind::BinOp(lhs, op, rhs) =>
                ExprKind::BinOp(Box::new(self.conv_expr(*lhs)), op, Box::new(self.conv_expr(*rhs))),
            parsetree::ExprKind::UnOp(op, expr) => ExprKind::UnOp(op, Box::new(self.conv_expr(*expr))),
            parsetree::ExprKind::Braced(expr) => self.conv_expr(*expr).value,
            parsetree::ExprKind::Var(var) => ExprKind::Var(self.conv_var(var)),
            parsetree::ExprKind::Call(call) => ExprKind::Call(match call {
                parsetree::Call::Normal(callee, args) => {
                    Call::Normal(Box::new(self.conv_expr(*callee)), self.conv_args(args))
                }
                parsetree::Call::Method(obj, name, args) => {
                    Call::Method(Box::new(self.conv_expr(*obj)), name, self.conv_args(args))
                }
            }),
            parsetree::ExprKind::Table(cons) => {
                ExprKind::Table(self.conv_table(cons))
            }
            parsetree::ExprKind::Func(func) => ExprKind::Func(self.conv_func(func)),
            parsetree::ExprKind::Array(elems) =>
                ExprKind::Array(elems.into_iter().map(|e| self.conv_expr(e)).collect()),
            parsetree::ExprKind::VarArgs => ExprKind::VarArgs,
        })
    }

    fn conv_table<'a>(&mut self, cons: parsetree::TableCons<'a>) -> Vec<(Expr<'a>, Expr<'a>)> {
        let mut entries: Vec<(Expr, Expr)> = Vec::new();
        let mut i = 0;  // XXX 1?
        for entry in cons {
            entries.push(match entry {
                parsetree::TableEntry::IdentPair(key, value) => {
                    (Spanned::new(key.span, ExprKind::Lit(Const::Str(key.value.into()))), self.conv_expr(value))
                }
                parsetree::TableEntry::Pair(key, value) => {
                    (self.conv_expr(key), self.conv_expr(value))
                }
                parsetree::TableEntry::Elem(value) => {
                    let idx = i;
                    i += 1;
                    (Spanned::new(value.span, ExprKind::Lit(Const::Number(idx.into()))), self.conv_expr(value))
                }
            });
        }

        entries
    }

    fn conv_args<'a>(&mut self, args: parsetree::CallArgs<'a>) -> Vec<Expr<'a>> {
        match args {
            parsetree::CallArgs::Normal(exprs) => {
                exprs.into_iter().map(|e| self.conv_expr(e)).collect()
            }
            parsetree::CallArgs::String(s) => {
                // TODO use real span
                vec![Spanned::default(ExprKind::Lit(Const::Str(s)))]
            }
            parsetree::CallArgs::Table(cons) => {
                vec![Spanned::default(ExprKind::Table(self.conv_table(cons)))]
            }
        }
    }
}

impl<'a> From<parsetree::Function<'a>> for Function<'a> {
    fn from(func: parsetree::Function<'a>) -> Self {
        let mut conv = AstConv;

        conv.conv_func(func)
    }
}
