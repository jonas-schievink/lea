//! Parse tree -> AST conversion

// TODO Do resolve and fold, possibly some lints in this pass

use super::*;
use parser::parsetree;
use parser::span::{Span, Spanned};

use lea_core::fndata::UpvalDesc;
use lea_core::Const;

use std::collections::HashMap;
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
            ConvResult::Two(t, t2) => ConvResult::Two(Spanned::new(span, t),
                                                      Spanned::new(span, t2)),
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

struct FuncData<'a> {
    /// Stack of lists of locals. Tracks all active scopes and thus all available locals.
    scopes: Vec<Vec<usize>>,
    /// Upvalue cache. Maps known upvalue names to their index in the function's `upvalues` list.
    /// If `None`, the upvalue cannot be resolved.
    upval_map: HashMap<&'a str, Option<usize>>,
    /// The function we're converting
    func: Function<'a>,
}

impl<'a> FuncData<'a> {
    /// Resolve a named local declared inside this function. Returns `Some(id)` when the local was
    /// found, `None` otherwise.
    fn resolve_local(&self, name: &'a str) -> Option<usize> {
        // Scan backwards through active scopes
        for scope in self.scopes.iter().rev() {
            for &local_id in scope {
                if *self.func.locals[local_id] == name {
                    debug!("resolved var '{}' as local {}", name, local_id);
                    return Some(local_id);
                }
            }
        }

        None
    }
}

struct AstConv<'a> {
    /// Function stack
    funcs: Vec<FuncData<'a>>,
}

impl<'a> AstConv<'a> {
    fn declare_local(&mut self, local: Spanned<&'a str>) -> usize {
        let fdata = self.funcs.last_mut().unwrap();
        let local_id = fdata.func.locals.len();
        fdata.func.locals.push(local);
        fdata.scopes.last_mut().unwrap().push(local_id);
        local_id
    }

    /// Resolves the given named variable
    fn resolve_var(&mut self, name: &'a str, span: Span) -> VarKind<'a> {
        if let Some(local) = self.funcs.last().unwrap().resolve_local(name) {
            return VarKind::Local(local);
        }

        // Find an upvalue
        let upval_id;
        if let Some(&res) = self.funcs.last().unwrap().upval_map.get(name) {
            debug!("upvalue cache hit: '{}' -> {:?}", name, res);
            upval_id = res;
        } else {
            // Resolve upvalue
            debug!("upvalue cache miss: '{}' (resolving now - nesting level {})",
                name, self.funcs.len() - 1);

            // Find a matching local in an enclosing function
            let mut level = 0;
            let mut upval_desc = None;
            for (i, data) in self.funcs.iter_mut().enumerate().rev().skip(1) {
                if let Some(local) = data.resolve_local(name) {
                    debug!("found upvalue '{}' as local {} in level {}", name, local, i);
                    level = i;
                    upval_desc = Some(UpvalDesc::Local(local));
                    break;
                }
                match data.upval_map.get(name) {
                    None => {}
                    Some(&None) => {
                        // Not an upvalue, early-out
                        break
                    }
                    Some(&Some(upval)) => {
                        debug!("found upvalue '{}' as upvalue {} in level {}", name, upval, i);
                        level = i;
                        upval_desc = Some(UpvalDesc::Upval(upval));
                        break;
                    }
                }
            }

            if let Some(mut desc) = upval_desc {
                // Found. Add an upvalue to all functions on the stack starting at level `level+1`
                // (since the upvalue was found at `level`)

                for (i, data) in self.funcs.iter_mut().enumerate().skip(level + 1) {
                    debug!("adding upvalue entry for '{}' to level {}: {:?}", name, i, desc);
                    let id = data.func.upvalues.len();
                    data.func.upvalues.push(desc);
                    assert!(data.upval_map.insert(name, Some(id)).is_none());

                    desc = UpvalDesc::Upval(id);
                }

                upval_id = Some(self.funcs.last().unwrap().func.upvalues.len() - 1);
            } else {
                upval_id = None;
                assert!(self.funcs.last_mut().unwrap().upval_map.insert(name, upval_id).is_none());
            }
        }

        match upval_id {
            Some(id) => VarKind::Upval(id),
            None => {
                // fall back to global access; resolve environment
                debug!("variable '{}' is a global", name);
                debug_assert!(name != "_ENV", "attempted to resolve '_ENV' as global");
                let envvar = Box::new(Spanned::new(span, self.resolve_var("_ENV", span)));

                VarKind::Indexed(envvar,
                    Box::new(Spanned::new(span, ExprKind::Lit(Const::Str(name.to_owned())))))
            }
        }
    }

    fn conv_func(&mut self, func: parsetree::Function<'a>) -> Function<'a> {
        let mut data = FuncData {
            scopes: Vec::new(),
            upval_map: HashMap::new(),
            func: Function {
                params: func.params.clone(),
                varargs: func.varargs,
                body: Block::new(Vec::new(), func.body.span),   // replaced anyways
                locals: Vec::new(),
                upvalues: Vec::new(),
            }
        };

        if self.funcs.is_empty() {
            // add implicit _ENV upvalue to main function.
            // (the UpvalDesc is ignored, so we can put anything there)
            data.func.upvalues.push(UpvalDesc::Upval(0));
            data.upval_map.insert("_ENV", Some(0));
        }

        self.funcs.push(data);

        let body = self.conv_block(func.body, &func.params);

        let mut func = self.funcs.pop().unwrap().func;
        func.body = body;
        func
    }

    fn conv_block(&mut self, block: parsetree::Block<'a>, locals: &[Spanned<&'a str>])
    -> Block<'a> {
        self.funcs.last_mut().unwrap().scopes.push(Vec::new());
        for local in locals {
            self.declare_local(*local);
        }

        let mut block = Block {
            span: block.span,
            stmts: block.stmts.into_iter().flat_map(|s| self.conv_stmt(s)).collect(),
            localmap: HashMap::new(),
        };

        // Need to update the block's `localmap`
        let scope = self.funcs.last_mut().unwrap().scopes.pop().unwrap();
        for id in scope {
            block.localmap.insert(*self.funcs.last().unwrap().func.locals[id], id);
        }

        block
    }

    fn conv_stmt(&mut self, stmt: parsetree::Stmt<'a>) -> ConvResult<Stmt<'a>> {
        match stmt.value {
            parsetree::StmtKind::Decl(names, exprs) => {
                for name in &names {
                    self.declare_local(*name);
                }

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
                ConvResult::One(StmtKind::Do(self.conv_block(block, &[])))
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
                let local_id = self.declare_local(local);
                ConvResult::Two(
                    StmtKind::Decl(vec![local], vec![]),
                    StmtKind::Assign(
                        vec![Spanned::new(local.span, VarKind::Local(local_id))],
                        vec![Spanned::new(func.body.span, ExprKind::Func(self.conv_func(func)))]
                    )
                )
            }
            parsetree::StmtKind::If { cond, body, elifs, el } => {
                // Build else block for this if statement
                // Start with "pure" else at the end
                let mut myelse: Option<Block> = el.map(|el| self.conv_block(el, &[]));

                myelse = elifs.into_iter().fold(myelse, |old, elif| {
                    let (cond, body) = elif.value;

                    Some(Block {
                        localmap: Default::default(),
                        span: elif.span,
                        stmts: vec![
                            Spanned::new(elif.span, StmtKind::If {
                                cond: self.conv_expr(cond),
                                body: self.conv_block(body, &[]),
                                el: old,
                            }),
                        ],
                    })
                });

                ConvResult::One(StmtKind::If {
                    cond: self.conv_expr(cond),
                    body: self.conv_block(body, &[]),
                    el: myelse,
                })
            }
            parsetree::StmtKind::While { cond, body } => {
                ConvResult::One(StmtKind::While {
                    cond: self.conv_expr(cond),
                    body: self.conv_block(body, &[]),
                })
            }
            parsetree::StmtKind::Repeat { abort_on, body } => {
                ConvResult::One(StmtKind::Repeat {
                    abort_on: self.conv_expr(abort_on),
                    body: self.conv_block(body, &[]),
                })
            }
            parsetree::StmtKind::For { var, start, step, end, body } => {
                ConvResult::One(StmtKind::For {
                    var: var,
                    start: self.conv_expr(start),
                    step: step.map(|e| self.conv_expr(e)),
                    end: self.conv_expr(end),
                    body: self.conv_block(body, &[var]),
                })
            }
            parsetree::StmtKind::ForIn { vars, iter, body } => {
                ConvResult::One(StmtKind::ForIn {
                    vars: vars.clone(),
                    iter: iter.into_iter().map(|e| self.conv_expr(e)).collect(),
                    body: self.conv_block(body, &vars),
                })
            }
        }.spanned(stmt.span)
    }

    fn conv_var(&mut self, var: parsetree::Variable<'a>) -> Variable<'a> {
        Spanned::new(var.span, match var.value {
            parsetree::VarKind::Named(name) => {
                self.resolve_var(name, var.span)
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

    fn conv_expr(&mut self, expr: parsetree::Expr<'a>) -> Expr<'a> {
        Spanned::new(expr.span, match expr.value {
            parsetree::ExprKind::Lit(lit) => ExprKind::Lit(lit),
            parsetree::ExprKind::BinOp(lhs, op, rhs) =>
                ExprKind::BinOp(Box::new(self.conv_expr(*lhs)), op, Box::new(self.conv_expr(*rhs))),
            parsetree::ExprKind::UnOp(op, expr) =>
                ExprKind::UnOp(op, Box::new(self.conv_expr(*expr))),
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

    fn conv_table(&mut self, cons: parsetree::TableCons<'a>) -> Vec<(Expr<'a>, Expr<'a>)> {
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

    fn conv_args(&mut self, args: parsetree::CallArgs<'a>) -> Vec<Expr<'a>> {
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
        let mut conv = AstConv {
            funcs: Vec::new(),
        };

        conv.conv_func(func)
    }
}
