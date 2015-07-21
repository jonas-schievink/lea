//! This module contains the logic used to resolve identifiers to their locals or globals.
//!
//! Internally, this module also performs the register allocation for all locals. This makes it
//! easy to reference upvalues (see `UpvalDesc` in program.rs) and makes the bytecode emitter do
//! less work.

// TODO better code style would be neat

use ast::*;
use ast::visit::*;
use parser::span::{Span, Spanned};

use lea_core::fndata::UpvalDesc;

use std::collections::HashMap;
use std::default::Default;


/// Data used by the resolver, associated to a function
struct FuncData<'a> {
    locals: Vec<Spanned<&'a str>>,

    /// Upvalues referenced from within the function
    upvals: Vec<UpvalDesc>,
    /// Maps known upvalue names to their id
    upval_map: HashMap<&'a str, usize>,

    /// Stack of lists of locals. Tracks all active scopes and thus all available locals.
    scopes: Vec<Vec<usize>>,
}

impl<'a> FuncData<'a> {
    fn new(locals: Vec<Spanned<&'a str>>) -> FuncData<'a> {
        FuncData {
            locals: locals,
            upvals: vec![],
            upval_map: Default::default(),
            scopes: vec![],
        }
    }

    /// Registers an upvalue and returns its id
    fn add_upval(&mut self, name: &'a str, desc: UpvalDesc) -> usize {
        let id = self.upvals.len();
        self.upvals.push(desc);
        self.upval_map.insert(name, id);

        id
    }

    /// Finds a reachable local (no upvalues are considered) with the given name and returns its id
    fn get_local(&self, name: &str) -> Option<usize> {
        // scan backwards through scopes
        let mut level = self.scopes.len() - 1;
        loop {
            for id in &self.scopes[level] {
                if name == self.locals[*id].value {
                    return Some(*id);
                }
            }

            if level == 0 { break; }
            level -= 1;
        }

        None
    }
}

/// A resolver will resolve any `VNamed` references in a function
struct Resolver<'a> {
    /// Stack of active functions
    funcs: Vec<FuncData<'a>>,
}

impl<'a> Resolver<'a> {
    /// Finds an upvalue in a parent function
    fn find_upval(&mut self, name: &'a str, userlvl: usize) -> Option<usize> {
        if userlvl == 0 {
            return None;
        }

        // search parent functions for locals / upvalues that match
        if let Some(id) = self.funcs[userlvl - 1].get_local(name) {
            return Some(self.funcs[userlvl].add_upval(name, UpvalDesc::Local(id)));
        } else {
            if let Some(id) = self.find_upval(name, userlvl - 1) {
                return Some(self.funcs[userlvl].add_upval(name, UpvalDesc::Upval(id)));
            } else {
                return None;
            }
        }

        None
    }

    /// Searches for an upvalue with the given name
    fn get_upval(&mut self, name: &'a str) -> Option<usize> {
        let level = self.funcs.len() - 1;

        {
            // search known upvals first
            let data = &self.funcs[level];
            if let Some(id) = data.upval_map.get(name) {
                return Some(*id);
            }
        }

        self.find_upval(name, level)
    }

    /// Declares a new local with the given name in the currently active scope and function.
    fn add_local(&mut self, name: Spanned<&'a str>) -> usize {
        let level = self.funcs.len() - 1;

        // this might be a redeclaration, in which case we ignore it (the emitter handles it)
        let func = &mut self.funcs[level];
        let scopelvl = func.scopes.len() - 1;

        {
            let scope = &func.scopes[scopelvl];
            for id in scope {
                if name.value == func.locals[*id].value {
                    // already declared
                    return *id;
                }
            }
        }

        // create new local
        let scope = &mut func.scopes[scopelvl];
        let id = func.locals.len();
        func.locals.push(name);
        scope.push(id);

        id
    }

    /// Resolves a block and declares a list of locals inside of it
    fn resolve_block(&mut self, mut b: Block<'a>, locals: Vec<Spanned<&'a str>>) -> Block<'a> {
        let level = self.funcs.len() - 1;
        self.funcs[level].scopes.push(vec![]);
        for name in locals {
            self.add_local(name);
        }

        b = walk_block(b, self);

        let data = &mut self.funcs[level];
        let scope = data.scopes.pop().unwrap();
        for id in scope {
            let name = &data.locals[id];
            b.localmap.insert(name.value, id);
        }

        b
    }

    /// Resolves the given named variable
    fn resolve_var(&mut self, name: &'a str, span: Span) -> _Variable<'a> {
        // first, try a local with that name
        if let Some(id) = self.funcs[self.funcs.len() - 1].get_local(name) {
            VLocal(id)
        } else {
            // find an upvalue
            if let Some(id) = self.get_upval(&name) {
                VUpval(id)
            } else {
                // fall back to global access; resolve environment
                let envvar = Box::new(Spanned::new(span,
                    self.resolve_var("_ENV", span)));

                VResGlobal(envvar, name.to_string())
            }
        }
    }
}

impl<'a> Transform<'a> for Resolver<'a> {
    fn visit_stmt(&mut self, mut s: Stmt<'a>) -> Stmt<'a> {
        s.value = match s.value {
            SDecl(names, mut exprs) => {
                exprs = exprs.into_iter().map(|e| walk_expr(e, self)).collect();

                for name in &names {
                    self.add_local(*name);
                }

                SDecl(names, exprs)
            },
            SLFunc(name, mut f) => {
                self.add_local(name);
                f = self.visit_func(f);
                SLFunc(name, f)
            },
            SFor{var, start, step, end, mut body} => {
                body = self.resolve_block(body, vec![var]);
                SFor{var: var, start: start, step: step, end: end, body: body}
            },
            SForIn{vars, iter, mut body} => {
                body = self.resolve_block(body, vars.clone());
                SForIn{vars: vars, iter: iter, body: body}
            },
            _ => { return walk_stmt(s, self); },
        };

        s
    }

    fn visit_var(&mut self, mut v: Variable<'a>) -> Variable<'a> {
        if let VNamed(name) = v.value {
            v.value = self.resolve_var(name, v.span);
            v
        } else {
            walk_var(v, self)
        }
    }

    fn visit_block(&mut self, b: Block<'a>) -> Block<'a> {
        self.resolve_block(b, vec![])
    }

    fn visit_func(&mut self, mut f: Function<'a>) -> Function<'a> {
        let mut data = FuncData::new(f.params.clone());
        if self.funcs.len() == 0 {
            // add implicit _ENV upvalue to main function.
            // (the UpvalDesc is ignored)
            data.add_upval("_ENV", UpvalDesc::Upval(0));
        }

        self.funcs.push(data);
        f.body = self.visit_block(f.body);

        let data = self.funcs.pop().unwrap();
        f.locals = data.locals;
        f.upvalues = data.upvals;

        f
    }
}

/// Resolves all locals used in the given main function and all functions defined within. This will
/// also resolve the environment of all functions.
///
/// All occurrences of `VNamed` will be converted to `VLocal`, `VUpval`, or `VResGlobal` after this
/// function returns.
pub fn resolve_func(f: Function) -> Function {
    Resolver {
        funcs: vec![],
    }.visit_func(f)
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::parse_main;

    use lea_ast::span::Spanned;
    use lea_ast::*;

    use lea_core::fndata::UpvalDesc;
    use lea_core::literal::*;

    use std::default::Default;

    macro_rules! localmap {
        () => {{
            ::std::collections::HashMap::<&str, usize>::new()
        }};
        ( $($key:ident: $e:expr),* ) => {{
            let mut m = ::std::collections::HashMap::<&str, usize>::new();
            $( m.insert(stringify!($key), $e); )*

            m
        }};
    }

    #[test]
    fn simple() {
        let mut f = parse_main(r#"
i = 0
local a
do
    local i
    local j = i
    i[j] = a
end
j = i
"#).unwrap();
        f = resolve_func(f);

        assert_eq!(f.body, Block::with_locals(vec![
            Spanned::default(SAssign(
                vec![Spanned::default(VResGlobal(Box::new(Spanned::default(VUpval(0))), "i".to_string()))],
                vec![Spanned::default(ELit(TInt(0)))],
            )),
            Spanned::default(SDecl(vec![Spanned::default("a")], vec![])),
            Spanned::default(SDo(Block::with_locals(vec![
                Spanned::default(SDecl(vec![Spanned::default("i")], vec![])),
                Spanned::default(SDecl(vec![Spanned::default("j")], vec![
                    Spanned::default(EVar(Spanned::default(VLocal(1)))),
                ])),
                Spanned::default(SAssign(
                    vec![Spanned::default(VIndex(
                        Box::new(Spanned::default(VLocal(1))),
                        Box::new(Spanned::default(EVar(Spanned::default(VLocal(2)))))
                    ))],
                    vec![Spanned::default(EVar(Spanned::default(VLocal(0))))],
                )),
            ], Default::default(), localmap!{ i: 1, j: 2 }))),
            Spanned::default(SAssign(
                vec![Spanned::default(
                    VResGlobal(Box::new(Spanned::default(VUpval(0))), "j".to_string())
                )],
                vec![Spanned::default(EVar(Spanned::default(
                    VResGlobal(Box::new(Spanned::default(VUpval(0))), "i".to_string())
                )))],
            )),
        ], Default::default(), localmap!{ a: 0 }));
    }

    #[test]
    fn complex() {
        let mut f = parse_main(r#"
local a
local function f()
    f = nil     -- upvalue
    local f = f -- both

    local function g() f = a end    -- chained upvalue + normal upvalue
end
"#).unwrap();
        f = resolve_func(f);

        assert_eq!(f, Function {
            params: vec![],
            varargs: true,
            locals: vec![Spanned::default("a"), Spanned::default("f")],
            upvalues: vec![UpvalDesc::Upval(0)],    // `_ENV`; the UpvalDesc is ignored
            body: Block::with_locals(vec![
                Spanned::default(SDecl(vec![Spanned::default("a")], vec![])),
                Spanned::default(SLFunc(Spanned::default("f"), Function {
                    params: vec![],
                    varargs: false,
                    locals: vec![Spanned::default("f"), Spanned::default("g")],
                    upvalues: vec![UpvalDesc::Local(1), UpvalDesc::Local(0)],
                    body: Block::with_locals(vec![
                        Spanned::default(SAssign(vec![
                            Spanned::default(VUpval(0))
                        ], vec![
                            Spanned::default(ELit(TNil))
                        ])),
                        Spanned::default(SDecl(vec![Spanned::default("f")], vec![
                            Spanned::default(EVar(Spanned::default(VUpval(0))))
                        ])),
                        Spanned::default(SLFunc(Spanned::default("g"), Function {
                            params: vec![],
                            varargs: false,
                            locals: vec![],
                            upvalues: vec![UpvalDesc::Local(0), UpvalDesc::Upval(1)],
                            body: Block::with_locals(vec![
                                Spanned::default(SAssign(vec![
                                    Spanned::default(VUpval(0))
                                ], vec![
                                    Spanned::default(EVar(Spanned::default(VUpval(1))))
                                ])),
                            ], Default::default(), localmap!{}),
                        })),
                    ], Default::default(), localmap!{ f: 0, g: 1 }),
                })),
            ], Default::default(), localmap!{ a: 0, f: 1 }),
        });
    }

    #[test]
    fn env_simple() {
        let mut f = parse_main("_ENV = 0").unwrap();
        f = resolve_func(f);

        assert_eq!(f.body, Block::new(vec![
            Spanned::default(SAssign(vec![
                Spanned::default(VUpval(0))
            ], vec![
                Spanned::default(ELit(TInt(0)))
            ])),
        ], Default::default()));
    }

    #[test]
    fn env_complex() {
        let mut f = parse_main(r#"
_ENV = nil
local _ENV
local function f() local _ENV i = nil end
local function g() _ENV = nil end
local function h() local function h1() r = nil end end
"#).unwrap();
        f = resolve_func(f);

        assert_eq!(f, Function {
            params: vec![],
            varargs: true,
            locals: vec![Spanned::default("_ENV"), Spanned::default("f"), Spanned::default("g"), Spanned::default("h")],
            upvalues: vec![UpvalDesc::Upval(0)],
            body: Block::with_locals(vec![
                Spanned::default(SAssign(vec![
                    Spanned::default(VUpval(0))
                ], vec![
                    Spanned::default(ELit(TNil))
                ])),
                Spanned::default(SDecl(vec![Spanned::default("_ENV")], vec![])),
                Spanned::default(SLFunc(Spanned::default("f"), Function {
                    params: vec![],
                    varargs: false,
                    locals: vec![Spanned::default("_ENV")],
                    upvalues: vec![],
                    body: Block::with_locals(vec![
                        Spanned::default(SDecl(vec![Spanned::default("_ENV")], vec![])),
                        Spanned::default(SAssign(vec![
                            Spanned::default(VResGlobal(
                                Box::new(Spanned::default(VLocal(0))), "i".to_string()
                            )),
                        ], vec![Spanned::default(ELit(TNil))])),
                    ], Default::default(), localmap!{ _ENV: 0 }),
                })),
                Spanned::default(SLFunc(Spanned::default("g"), Function {
                    params: vec![],
                    varargs: false,
                    locals: vec![],
                    upvalues: vec![UpvalDesc::Local(0)],
                    body: Block::with_locals(vec![
                        Spanned::default(SAssign(vec![
                            Spanned::default(VUpval(0))
                        ], vec![
                            Spanned::default(ELit(TNil))
                        ])),
                    ], Default::default(), localmap!{}),
                })),
                Spanned::default(SLFunc(Spanned::default("h"), Function {
                    params: vec![],
                    varargs: false,
                    locals: vec![Spanned::default("h1")],
                    upvalues: vec![UpvalDesc::Local(0)],
                    body: Block::with_locals(vec![
                        Spanned::default(SLFunc(Spanned::default("h1"), Function {
                            params: vec![],
                            varargs: false,
                            locals: vec![],
                            upvalues: vec![UpvalDesc::Upval(0)],
                            body: Block::new(vec![
                                Spanned::default(SAssign(vec![
                                    Spanned::default(VResGlobal(
                                        Box::new(Spanned::default(VUpval(0))), "r".to_string()
                                    ))
                                ], vec![
                                    Spanned::default(ELit(TNil)),
                                ])),
                            ], Default::default()),
                        })),
                    ], Default::default(), localmap!{ h1: 0 }),
                })),
            ], Default::default(), localmap!{ _ENV: 0, f: 1, g: 2, h: 3 }),
        });
    }
}
