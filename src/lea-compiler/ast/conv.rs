//! Parse tree -> AST conversion

use super::*;
use parser::parsetree;
use parser::span::Spanned;

use ::lea_core::literal;

impl<'a> From<parsetree::Function<'a>> for Function<'a> {
    fn from(func: parsetree::Function<'a>) -> Self {
        Function {
            params: func.params,
            varargs: func.varargs,
            body: func.body.into(),
            locals: Default::default(),
            upvalues: Default::default(),
        }
    }
}

impl<'a> From<parsetree::Block<'a>> for Block<'a> {
    fn from(block: parsetree::Block<'a>) -> Self {
        Block {
            span: block.span,
            stmts: conv_vec(block.stmts),
            localmap: Default::default(),
        }
    }
}

impl<'a> From<parsetree::_Variable<'a>> for _Variable<'a> {
    fn from(var: parsetree::_Variable<'a>) -> Self {
        match var {
            parsetree::VNamed(name) => {
                VNamed(name)
            }
            parsetree::VIndex(var, index) => {
                VIndex(Box::new(spanned_into(*var)), match index {
                    parsetree::VarIndex::DotIndex(name) => {
                        Box::new(Spanned::new(name.span,
                            ELit(literal::TStr(name.value.to_owned()))))
                    }
                    parsetree::VarIndex::ExprIndex(expr) => {
                        Box::new(spanned_into(*expr))
                    }
                })
            }
        }
    }
}

impl<'a> From<parsetree::TableEntry<'a>> for TableEntry<'a> {
    fn from(entry: parsetree::TableEntry<'a>) -> Self {
        match entry {
            parsetree::TableEntry::Pair(key, value) => {
                TableEntry::Pair(spanned_into(key), spanned_into(value))
            }
            parsetree::TableEntry::Elem(value) => {
                TableEntry::Elem(spanned_into(value))
            }
        }
    }
}

impl<'a> From<parsetree::_Expr<'a>> for _Expr<'a> {
    fn from(expr: parsetree::_Expr<'a>) -> Self {
        match expr {
            parsetree::ELit(lit) => ELit(lit),
            parsetree::EBinOp(lhs, op, rhs) =>
                EBinOp(Box::new(spanned_into(*lhs)), op, Box::new(spanned_into(*rhs))),
            parsetree::EUnOp(op, expr) => EUnOp(op, Box::new(spanned_into(*expr))),
            parsetree::EBraced(expr) => EBraced(Box::new(spanned_into(*expr))),

            parsetree::EVar(var) => EVar(spanned_into(var)),

            parsetree::ECall(call) => ECall(match call {
                parsetree::SimpleCall(callee, args) => {
                    SimpleCall(Box::new(spanned_into(*callee)), conv_args(args))
                }
                parsetree::MethodCall(obj, name, args) => {
                    MethodCall(Box::new(spanned_into(*obj)), name, conv_args(args))
                }
            }),

            parsetree::EFunc(func) => EFunc(func.into()),

            parsetree::ETable(cons) =>
                ETable(cons.into_iter().map(|entry| entry.into()).collect()),
            parsetree::EArray(elems) => EArray(vec_into(elems)),
            parsetree::EVarArgs => EVarArgs,
        }
    }
}

/// Implemented for parse tree nodes. Converts them into `n` instances of `Target`.
trait Conv<'a> {
    type Target;

    fn conv<F>(self, push: F) where F: FnMut(Self::Target);
}

fn conv_vec<'a, T: Conv<'a>>(v: Vec<Spanned<T>>) -> Vec<Spanned<T::Target>> {
    let mut res = Vec::new();
    for item in v.into_iter() {
        let span = item.span;
        item.value.conv(|i| res.push(Spanned::new(span, i)));
    }

    res
}

fn conv_args<'a>(args: parsetree::CallArgs<'a>) -> Vec<Expr<'a>> {
    let mut v = Vec::new();
    args.conv(|e| v.push(e));

    v
}

fn vec_into<'a, U, T: Into<U>>(v: Vec<Spanned<T>>) -> Vec<Spanned<U>> {
    v.into_iter().map(|item| spanned_into(item)).collect()
}

// Needed because Spanned<T> cannot impl Into. Not even specialization can help there.
fn spanned_into<U, T: Into<U>>(item: Spanned<T>) -> Spanned<U> {
    Spanned::new(item.span, item.value.into())
}

impl<'a> Conv<'a> for parsetree::_Stmt<'a> {
    type Target = _Stmt<'a>;

    fn conv<F>(self, mut push: F) where F: FnMut(Self::Target) {
        match self {
            parsetree::SDecl(names, exprs) => {
                push(_Stmt::SDecl(names, vec_into(exprs)));
            }

            parsetree::SAssign(vars, exprs) => {
                push(_Stmt::SAssign(vec_into(vars), vec_into(exprs)));
            }

            parsetree::SDo(block) => {
                push(_Stmt::SDo(block.into()));
            }

            parsetree::SBreak => {
                push(_Stmt::SBreak);
            }

            parsetree::SReturn(exprs) => {
                push(_Stmt::SReturn(vec_into(exprs)));
            }

            parsetree::SCall(call) => {
                push(_Stmt::SCall(match call {
                    parsetree::SimpleCall(callee, args) => {
                        SimpleCall(Box::new(spanned_into(*callee)), conv_args(args))
                    }
                    parsetree::MethodCall(obj, name, args) => {
                        MethodCall(Box::new(spanned_into(*obj)), name, conv_args(args))
                    }
                }));
            }

            parsetree::SFunc(var, func) => {
                push(_Stmt::SFunc(spanned_into(var), func.into()));
            }

            parsetree::SMethod(var, name, func) => {
                push(_Stmt::SMethod(spanned_into(var), name, func.into()));
            }

            parsetree::SLFunc(local, func) => {
                push(_Stmt::SLFunc(local, func.into()));
            }

            parsetree::SIf { cond, body, el } => {
                push(_Stmt::SIf {
                    cond: spanned_into(cond),
                    body: body.into(),
                    el: el.into(),
                });
            }

            parsetree::SWhile { cond, body } => {
                push(_Stmt::SWhile {
                    cond: spanned_into(cond),
                    body: body.into(),
                });
            }

            parsetree::SRepeat { abort_on, body } => {
                push(_Stmt::SRepeat {
                    abort_on: spanned_into(abort_on),
                    body: body.into(),
                });
            }

            parsetree::SFor { var, start, step, end, body } => {
                push(_Stmt::SFor {
                    var: var,
                    start: spanned_into(start),
                    step: step.map(|e| spanned_into(e)),
                    end: spanned_into(end),
                    body: body.into(),
                });
            }

            parsetree::SForIn { vars, iter, body } => {
                push(_Stmt::SForIn {
                    vars: vars,
                    iter: vec_into(iter),
                    body: body.into(),
                });
            }
        }
    }
}

impl<'a> Conv<'a> for parsetree::CallArgs<'a> {
    type Target = Expr<'a>;

    fn conv<F>(self, mut push: F) where F: FnMut(Self::Target) {
        match self {
            parsetree::CallArgs::Normal(exprs) => {
                for expr in exprs {
                    push(spanned_into(expr))
                }
            }
            parsetree::CallArgs::String(s) => {
                // TODO use real span
                push(Spanned::default(ELit(literal::TStr(s))))
            }
            parsetree::CallArgs::Table(cons) => {
                push(Spanned::default(ETable(
                    cons.into_iter().map(|entry| entry.into()).collect()
                )))
            }
        }
    }
}
