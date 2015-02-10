//! This module contains the logic used to resolve identifiers to their locals or globals.

use ast::*;
use visit::*;

use std::mem;
use std::collections::HashMap;

use self::LocalRef::*;

enum LocalRef {
    /// Local declared in the current block
    Owned(usize),
    /// Local declared in a parent block
    /// id, parent block level (0 = direct parent, 1 = parent of parent, ...)
    Outer(usize, usize),
    /// Upvalue with the given id defined in the parent function
    Upvalue(usize),
}

/// A resolver will work on a single scope and resolve any `VNamed` references
struct Resolver<'a> {
    /// Set of locals reachable from within the currently resolved block (contains all locals from
    /// parent scopes)
    reachable: HashMap<String, LocalRef>,
    /// Set of locals declared within this block. Subset of `reachable`.
    owned: HashMap<String, usize>,
    /// Locals owned by (declared in) the current block
    local_vec: Vec<String>,
    /// Resolver of the parent function. Knows all locals available to this function as upvalues.
    pfunc: Option<&'a Resolver<'a>>,
}

impl <'a> Resolver<'a> {
    fn is_local_reachable(&mut self, name: &str) -> bool {
        self.reachable.get(name).is_some()
    }

    fn is_upvalue(&mut self, name: &String) -> bool {
        // Ask the parent function's resolver to search for an available upvalue
        // This recursively registers the upvalue as one, should some function up the stack have a
        // local with the correct name.
        false
    }

    fn add_local(&mut self, name: String) {
        let id = self.local_vec.len();
        self.local_vec.push(name.clone());

        self.owned.insert(name.clone(), id);
        self.reachable.insert(name, Owned(id));
    }

    fn resolve_var(&mut self, v: &mut Variable) {
        match v.value {
            VNamed(..) => {
                let mut name = String::new();

                if let VNamed(ref mut vname) = v.value {
                    mem::swap(vname, &mut name);
                }

                mem::replace(&mut v.value, if self.is_local_reachable(&name) {
                    VLocal(name)
                } else {
                    VGlobal(name)
                });
            },
            VIndex(..) => {
                walk_var(v, self);
            },
            _ => {
                panic!("unexpected variable: {:?}", v.value);
            }
        }
    }

    /// Resolves a block and defines a list of locals that can be used inside the block
    fn resolve_with_locals(&self, b: &mut Block, locals: Vec<String>) {
        let mut res = Resolver {
            reachable: HashMap::new(),
            owned: HashMap::new(),
            local_vec: Vec::new(),
            pfunc: self.pfunc,
        };

        for name in locals {
            res.add_local(name);
        }

        resolve_block_with(b, res);
    }
}

impl <'a> Visitor for Resolver<'a> {
    fn visit_stmt(&mut self, s: &mut Stmt) {
        match s.value {
            SDecl(ref mut names, ref mut exprs) => {
                for name in names {
                    self.add_local(name.clone());
                }

                for expr in exprs {
                    walk_expr(expr, self);
                }
            },
            SFor{ref var, ref mut body, ..} => {
                self.resolve_with_locals(body, vec![var.clone()]);
            },
            SForIn{ref vars, ref mut body, ..} => {
                self.resolve_with_locals(body, vars.clone());
            },
            _ => {
                walk_stmt(s, self);
            }
        }
    }

    fn visit_var(&mut self, v: &mut Variable) {
        self.resolve_var(v);
    }

    fn visit_block(&mut self, b: &mut Block) {
        let mut res = Resolver {
            reachable: HashMap::new(),
            owned: HashMap::new(),
            local_vec: Vec::new(),
            pfunc: self.pfunc,
        };

        for (name, localref) in &self.reachable {
            res.reachable.insert(name.clone(), match *localref {
                Owned(id) => {
                    // Child blocks can access our locals as outer locals
                    Outer(id, 0)
                },
                Outer(id, lvl) => {
                    // Increase block level
                    Outer(id, lvl + 1)
                },
                Upvalue(id) => {
                    // Upvalues already imported stay this way until the function is left
                    Upvalue(id)
                },
            });
        }

        resolve_block_with(b, res);
    }

    fn visit_func(&mut self, f: &mut Function) {
        // TODO
        self.visit_block(&mut f.value.body);
    }
}

fn resolve_block_with<'a>(b: &mut Block, mut res: Resolver<'a>) {
    walk_block(b, &mut res);

    b.locals = res.local_vec;
}

/// Resolves all locals used in the given block. Recursively resolves all blocks found inside.
///
/// Allows blocks inside the given block to access locals declared within the parent block
/// (assuming they are declared before the block). Does not allow the given block to access outer
/// locals.
pub fn resolve_block(b: &mut Block) {
    let res = Resolver {
        reachable: HashMap::new(),
        owned: HashMap::new(),
        local_vec: Vec::new(),
        pfunc: None,    // Assume no parent function
    };

    resolve_block_with(b, res);
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::block;
    use span::Spanned;
    use ast::*;

    use std::default::Default;

    #[test]
    fn test() {
        let mut b = block(r#"
i = 0
local a
do
    local i
    local j = i
    i[j] = a
end
j = i
"#).unwrap();
        resolve_block(&mut b);

        assert_eq!(b, Block::with_locals(vec![
            Spanned::default(SAssign(
                vec![Spanned::default(VGlobal("i".to_string()))],
                vec![Spanned::default(ELit(TInt(0)))],
            )),
            Spanned::default(SDecl(vec!["a".to_string()], vec![])),
            Spanned::default(SDo(Block::with_locals(vec![
                Spanned::default(SDecl(vec!["i".to_string()], vec![])),
                Spanned::default(SDecl(vec!["j".to_string()], vec![
                    Spanned::default(EVar(Spanned::default(VLocal("i".to_string())))),
                ])),
                Spanned::default(SAssign(
                    vec![Spanned::default(VIndex(
                        Box::new(Spanned::default(VLocal("i".to_string()))),
                        Box::new(Spanned::default(EVar(Spanned::default(VLocal("j".to_string())))))
                    ))],
                    vec![Spanned::default(EVar(Spanned::default(VLocal("a".to_string()))))],
                )),
            ], vec!["i".to_string(), "j".to_string()], Default::default()))),
            Spanned::default(SAssign(
                vec![Spanned::default(VGlobal("j".to_string()))],
                vec![Spanned::default(EVar(Spanned::default(VGlobal("i".to_string()))))],
            )),
        ], vec!["a".to_string()], Default::default()));
    }
}
