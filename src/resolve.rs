//! This module contains the logic used to resolve identifiers to their locals or globals.
//!
//! Internally, this module also performs the register allocation for all locals. This makes it
//! easy to reference upvalues (see `UpvalDesc` in program.rs) and makes the bytecode emitter do
//! less work.


use ast::*;
use visit::*;
use program::UpvalDesc;

use std::mem;
use std::collections::HashMap;
use std::default::Default;

use self::LocalRef::*;


/// Describes how a local can be reached
#[derive(PartialEq, Eq, Copy, Debug)]
enum LocalRef {
    /// Local declared in the current block or a parent block in the same function. The `usize` is
    /// the stack slot allocated to the local.
    Local(usize),
    /// Upvalue defined in some active scope of the parent function (`pfunc`). The `usize` is an
    /// index into the `upvalues` vector of the function that references the upval.
    Upvalue(usize),
}

/// A resolver will work on a single scope and resolve any `VNamed` references
struct Resolver<'a> {
    /// Caches locals that were already looked up. Newly declared locals also get an entry here.
    reachable: HashMap<String, LocalRef>,

    /// Set of locals declared within this block. Maps local names to their stack slot.
    /// Note that the lowest stack slot isn't always 0 (since parent blocks allocate first).
    owned: HashMap<String, usize>,

    /// Id assigned to the next local declared in the current block. This is the next free stack
    /// slot in this function.
    next_id: usize,

    /// Resolver of outer block inside the current function, or `None` if this is the topmost block
    /// of the function.
    pblock: Option<&'a Resolver<'a>>,

    /// Reference to the function we resolve for
    func: &'a mut Function,

    /// Parent function's resolver and a reference to the parent function
    pfunc: Option<&'a mut Resolver<'a>>,
}

impl <'a> Resolver<'a> {
    fn new(pblock: Option<&'a Resolver<'a>>, pfunc: Option<&'a mut Resolver<'a>>,
    func: &'a mut Function, start: usize) -> Resolver<'a> {
        Resolver {
            reachable: Default::default(),
            owned: Default::default(),
            next_id: start,
            pblock: pblock,
            func: func,
            pfunc: pfunc,
        }
    }

    /// Tries to find a local with the given name. Looks up the `reachable` cache first. If not
    /// found, searches the parent scope(s), then the parent function(s) (if existing). If the
    /// local was found, adds an entry for it to the `reachable` map and returns a copy of the
    /// `LocalRef`. Otherwise, returns None.
    fn lookup(&mut self, name: &str) -> Option<LocalRef> {
        if let Some(l) = self.reachable.get(name) {
            // Cache hit, return a copy
            Some(*l)
        } else {
            // Locals declared inside the current scope are added automatically, no need to search
            // them. Search containing scopes (parents) first:
            if let Some(parent) = self.pblock {
                if let Some(l) = parent.lookup(name) {
                    // Parent block found the local, add cache entry
                    self.reachable.insert(String::from_str(name), l);
                    return Some(l);
                }
            }

            // Not a parent block local. Might be Upvalue: Search parent function.
            if let Some(pfunc) = self.pfunc {
                if let Some(l) = pfunc.lookup(name) {
                    // Found as Upvalue or Local in the parent function
                    return match l {
                        Local(slot) => {
                            let upvalId = self.func.upvalues.len();
                            print!("new upval `{}` (id {}) from stack slot {}", name, upvalId, slot);
                            // Register our upvalue
                            self.func.upvalues.push(UpvalDesc::Stack(slot));

                            let lref = Upvalue(upvalId);
                            self.reachable.insert(String::from_str(name), lref);
                            Some(lref)
                        },
                        Upvalue(id) => {
                            // Chained upvalue. The parent's resolve method has already registered
                            // the upvalue, so we don't have to do much:
                            let upvalId = self.func.upvalues.len();
                            print!("new upvalue `{}` (id {}) from parent upvalue {}", name, upvalId, id);
                            self.func.upvalues.push(UpvalDesc::Upval(id));

                            let lref = Upvalue(upvalId);
                            self.reachable.insert(String::from_str(name), lref);
                            Some(lref)
                        },
                    };
                }
            }

            // No matching upvalue found. Defer to global access.
            None
        }
    }

    /// Adds a local with the given name, making it reachable.
    fn add_local(&mut self, name: String) {
        print!("Adding local `{}`; stack slot {}", name, self.next_id);

        let id = self.owned.len() + self.next_id;

        self.owned.insert(name.clone(), id);
        self.reachable.insert(name, Local(id));
        self.next_id += 1;
    }

    fn resolve_var(&mut self, v: &mut Variable) {
        match v.value {
            VNamed(..) => {
                let mut name = String::new();

                if let VNamed(ref mut vname) = v.value {
                    mem::swap(vname, &mut name);
                }

                mem::replace(&mut v.value, match self.lookup(&name) {
                    None => {
                        VGlobal(name)
                    },
                    Some(Local(slot)) => {
                        VLocal(slot)
                    },
                    Some(Upvalue(id)) => {
                        VUpval(id)
                    }
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
}

fn resolve_block_with<'a>(b: &mut Block, mut res: Resolver<'a>) {
    walk_block(b, &mut res);

    b.localmap = res.owned;
}

/// Resolves a block inside the resolver's scope and defines a list of locals that can be used
/// inside the block
fn resolve_with_locals<'a>(res: &Resolver<'a>, b: &mut Block, locals: Vec<String>) {
    let mut newres = Resolver::new(Some(res), res.pfunc, res.func, res.next_id);

    for name in locals {
        newres.add_local(name);
    }

    resolve_block_with(b, newres);

    res.next_id = newres.next_id;
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
                resolve_with_locals(self, body, vec![var.clone()]);
            },
            SForIn{ref vars, ref mut body, ..} => {
                resolve_with_locals(self, body, vars.clone());
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
            pfunc: self.pfunc,
        };

        resolve_block_with(b, res);
    }

    fn visit_func(&mut self, f: &mut Function) {
        // TODO
        self.visit_block(&mut f.value.body);
    }
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
