//! This module implements the byte code emitter.

#![allow(dead_code)]

use compiler::ast::*;
use compiler::visit::*;
use opcode::*;
use program::FnData;
use limits;

use std::{u8, u16};
use std::collections::HashMap;
use std::cmp;


#[derive(Clone, Debug)]
pub struct EmitError {
    pub msg: &'static str,
    pub detail: Option<String>,
}

/// On success, the main function is returned as an `FnData` object. On error, the `EmitError`s
/// produced are returned.
pub type EmitResult = Result<FnData, Vec<EmitError>>;

struct Emitter<'a> {
    source_name: String,
    /// List of errors that have occurred while emitting. They are ignored until the main function
    /// is traversed. If this list isn't empty when the emitter is done, it will not complete the
    /// process and return an error to the caller instead.
    errs: Vec<EmitError>,
    /// Function stack. The last entry is the currently emitted function. When done emitting, the
    /// last `FnData` is popped off and added to the child function list of the parent.
    funcs: Vec<FnData>,

    block: Option<&'a Block>,
    alloc: HashMap<usize, u8>,
}

impl <'a> Emitter<'a> {
    fn new(source_name: &str) -> Emitter<'a> {
        Emitter {
            source_name: source_name.to_string(),
            errs: Vec::new(),
            funcs: Vec::with_capacity(4),
            block: None,
            alloc: HashMap::with_capacity(8),
        }
    }

    fn cur_block(&self) -> &'a Block {
        match self.block {
            Some(b) => b,
            None => panic!("emitter has no current block"),
        }
    }

    /// Get the FnData of the currently emitted function
    fn cur_func(&mut self) -> &mut FnData {
        let len = self.funcs.len();
        debug_assert!(len > 0);
        &mut self.funcs[len-1]
    }

    /// Gets the slot the given local is allocated into. Panics if the local is not in the current
    /// block/scope.
    fn get_slot(&self, local_id: usize) -> u8 {
        *self.alloc.get(&local_id).unwrap()
    }

    /// Allocates `count` stack slots. Returns the lowest one that was allocated (slots are
    /// allocated consecutively).
    fn alloc_slots(&mut self, count: usize) -> u8 {
        let stacksz = self.cur_func().stacksize;
        if count + stacksz as usize >= u8::MAX as usize {
            self.err("stack limit reached", Some(format!(
                "limit {} was reached when attempting to allocate {} slots", u8::MAX, count)));
            u8::MAX
        } else {
            self.cur_func().stacksize += count as u8;
            stacksz
        }
    }

    fn dealloc_slots(&mut self, count: usize) {
        debug_assert!(self.cur_func().stacksize as usize >= count);
        self.cur_func().stacksize -= count as u8;
    }

    /// Adds a constant to the program's constant table and returns its index (does not add it if
    /// the same value already exists in the constant table).
    fn add_const(&mut self, lit: &Literal) -> u16 {
        // ensure that only "useful" constant are added
        debug_assert!(match *lit {
            TInt(..) | TFloat(..) | TStr(..) => true,
            TBool(..) | TNil => false, // handled by special opcodes
        });

        {
            let func = self.cur_func();
            for i in 0..func.consts.len() {
                let c = &func.consts[i];
                if lit == c {
                    // this cast cannot fail, because we stop adding constants when the u16 limit
                    // is reached
                    return i as u16;
                }
            }
        }

        let id = self.cur_func().consts.len();
        if id > u16::MAX as usize {
            self.err("constant limit reached", Some(format!("limit: {}", u16::MAX as u64 + 1)));
            u16::MAX
        } else {
            println!(":CONST {} = {:?}", id, lit);
            self.cur_func().consts.push(lit.clone());
            id as u16
        }
    }

    /// Adds an error to the error list. The emitter ignores errors until it is done. If the error
    /// list isn't empty when the main function was traversed, the emitter will return an error to
    /// the caller.
    fn err(&mut self, msg: &'static str, detail: Option<String>) {
        self.errs.push(EmitError {
            msg: msg,
            detail: detail,
        });
    }

    fn get_result(mut self) -> EmitResult {
        if self.errs.len() > 0 {
            Err(self.errs)
        } else {
            assert_eq!(self.funcs.len(), 1);

            Ok(self.funcs.pop().unwrap())
        }
    }

    /// Emits an opcode into the current function and returns its "address" or index
    fn emit(&mut self, op: Opcode) -> usize {
        println!("=> {:?}", op);

        let idx = self.cur_func().opcodes.len();
        if idx as u64 >= limits::OP_LIMIT {
            self.err("opcode limit reached", Some(format!("limit: {}", limits::OP_LIMIT)));
        } else {
            self.cur_func().opcodes.push(op);
        }

        idx
    }

    fn emit_nil(&mut self, slot: u8) -> u8 {
        self.emit(LOADNIL(slot, 0));
        slot
    }

    fn emit_expr_or_nil(&mut self, e: Option<&'a Expr>, hint_slot: u8) -> u8 {
        match e {
            Some(ref e) => self.emit_expr(e, hint_slot),
            None => self.emit_nil(hint_slot),
        }
    }

    /// Emits an assignment to a variable. The function `f` is called with an emitter reference and
    /// a slot hint to get the slot the assigned value is located in.
    fn emit_assign<F>(&mut self, target: &'a Variable, f: F)
    where F: FnOnce(&mut Emitter<'a>, u8) -> u8 {
        match target.value {
            VLocal(id) => {
                let slot = self.get_slot(id);
                let realslot = f(self, slot);

                if realslot != slot {
                    self.emit(MOV(slot, realslot));
                }
            }

            VUpval(id) => {
                if id > u8::MAX as usize {
                    self.err("upvalue limit reached", Some(format!(
                        "upvalue #{} over limit {}", id, u8::MAX)));
                } else {
                    let valslot = self.alloc_slots(1);
                    let valslot = f(self, valslot);

                    self.emit(SETUPVAL(id as u8, valslot));

                    self.dealloc_slots(1);
                }
            }

            VIndex(ref v, ref idx) => {
                let slot = self.alloc_slots(1);
                let slot = self.emit_var(v, slot);
                let idxslot = self.alloc_slots(1);
                let idxslot = self.emit_expr(idx, idxslot);

                let valslot = self.alloc_slots(1);
                let valslot = f(self, valslot);

                self.emit(SETIDX(slot, idxslot, valslot));

                self.dealloc_slots(3);
            }

            VResGlobal(ref var, ref strn) | VDotIndex(ref var, ref strn) => {
                let slot = self.alloc_slots(1);
                let slot = self.emit_var(&*var, slot);
                let const_id = self.add_const(&TStr(strn.clone()));
                let str_slot = self.alloc_slots(1);

                let valslot = self.alloc_slots(1);
                let valslot = f(self, valslot);

                self.emit(LOADK(str_slot, const_id));
                self.emit(SETIDX(slot, str_slot, valslot));

                self.dealloc_slots(3);
            },

            VGlobal(_) => panic!("VGlobal encountered by emitter, resolver is broken"),
            VNamed(_) => panic!("VNamed encountered by emitter, resolver is broken"),
        }
    }

    /// Emits a variable used as an expression
    fn emit_var(&mut self, v: &'a Variable, hint_slot: u8) -> u8 {
        match v.value {
            VLocal(id) => self.get_slot(id),    // ignores hint
            VUpval(id) => {
                if id > u8::MAX as usize {
                    self.err("upvalue limit reached", Some(format!("upvalue #{} over limit {}",
                        id, u8::MAX)));
                } else {
                    self.emit(GETUPVAL(hint_slot, id as u8));
                }
                hint_slot
            },

            VResGlobal(ref var, ref strn) | VDotIndex(ref var, ref strn) => {
                let var_slot = self.emit_var(&*var, hint_slot);
                let const_id = self.add_const(&TStr(strn.clone()));
                let str_slot = self.alloc_slots(1);

                self.emit(LOADK(str_slot, const_id));
                self.emit(GETIDX(hint_slot, var_slot, str_slot));

                self.dealloc_slots(1);
                hint_slot
            },

            /// References an indexed variable (a field)
            VIndex(ref var, ref idx) => {
                let var_slot = self.alloc_slots(1);
                let var_slot = self.emit_var(&*var, var_slot);
                let idx_slot = self.alloc_slots(1);
                let idx_slot = self.emit_expr(&*idx, idx_slot);

                self.emit(GETIDX(hint_slot, var_slot, idx_slot));

                self.dealloc_slots(2);
                hint_slot
            },

            VGlobal(_) => panic!("VGlobal encountered by emitter, resolver is broken"),
            VNamed(_) => panic!("VNamed encountered by emitter, resolver is broken"),
        }
    }

    /// Emits an expression that can return multiple values. Up to `max_res` values will be stored
    /// in the stack slots starting at `start_slot`.
    ///
    /// If `max_res` is 0, any number of results is stored. The stack will be automatically
    /// expanded by the VM.
    ///
    /// The caller has to allocate `max_res` slots starting at and including `start_slot` (if
    /// `max_res` is 0, `start_slot` can point to the next unallocated stack slot, since the VM
    /// dynamically allocates the stack slots needed).
    ///
    /// This method makes sure that all unused slots are filled with nil, either at runtime
    /// (handled by the VM), or by emitting LOADNIL instructions to fill all `max_res` slots.
    fn emit_expr_multi(&mut self, e: &'a Expr, start_slot: u8, max_res: u8) {
        match e.value {
            ECall(_) => {
                unimplemented!();   // TODO
            }
            EVarArgs => {
                self.emit(VARARGS(start_slot, max_res));
            }
            _ => {
                // These all have a single result
                let mut target = start_slot;
                if max_res == 0 {
                    // no actual slot allocated!
                    target = self.alloc_slots(1);
                }

                let slot = self.emit_expr(e, target);
                if slot != start_slot {
                    self.emit(MOV(target, slot));
                }

                if max_res == 0 {
                    self.dealloc_slots(1);
                }

                if max_res > 1 {
                    self.emit(LOADNIL(start_slot+1, max_res-2));
                }
            }
        }
    }

    /// Emits an expression and returns the stack slot it was emitted into. Tries to emit it to
    /// `hint_slot` (which must be allocated by the caller).
    ///
    /// Expressions that can result in multiple values (calls, varargs) are adjusted to a single
    /// result.
    fn emit_expr(&mut self, e: &'a Expr, hint_slot: u8) -> u8 {
        match e.value {
            ELit(ref lit) => {
                match *lit {
                    TNil => {
                        self.emit(LOADNIL(hint_slot, 0));
                        hint_slot
                    }
                    TInt(_) | TFloat(_) | TStr(_) => {
                        let id = self.add_const(lit);
                        self.emit(LOADK(hint_slot, id));
                        hint_slot
                    }
                    TBool(b) => {
                        self.emit(LOADBOOL(hint_slot, 0, b));
                        hint_slot
                    }
                }
            }
            EVar(ref var) => self.emit_var(var, hint_slot),
            _ => panic!("NYI expression {:?}", e),  // TODO remove
        }
    }
}

impl <'a> Visitor<'a> for Emitter<'a> {
    fn visit_stmt(&mut self, s: &'a Stmt) {
        match s.value {
            SDecl(ref names, ref exprs) => {
                let mut it = exprs.iter();
                for name in names {
                    let id = self.cur_block().get_local(name).unwrap();
                    let slot = self.get_slot(*id);

                    match it.next() {
                        Some(e) => {
                            let real_slot = self.emit_expr(e, slot);
                            if slot != real_slot { self.emit(MOV(slot, real_slot)); }
                        }
                        None => {
                            self.emit(LOADNIL(slot, 0));
                        }
                    }
                }

                // excess expressions must be evaluated as well
                for rest in it {
                    let slot = self.alloc_slots(1);
                    self.emit_expr(rest, slot);
                    self.dealloc_slots(1);
                }
            }
            SAssign(ref vars, ref vals) => {
                // i, j = a, b, c
                // a -> tmp0
                // b -> j
                // tmp0 -> i
                // c -> /dev/null
                //
                // i, j, k = a
                // a -> i
                // nil -> j
                // nil -> k
                //
                // i, j, k = a, b
                // a -> tmp0
                // b -> j
                // nil -> k
                // tmp0 -> i
                //
                // i, j, k = a, b, c
                // a -> tmp0
                // b -> tmp1
                // c -> k
                // tmp0 -> i
                // tmp1 -> j
                //
                // i = a, b
                // a -> i
                // b -> /dev/null
                //
                // ======
                //
                // Need `min(varcount-1, valcount-1)` temps (= `tmpcount`). Eval first `tmpcount`
                // expressions on the right-hand side into the temps (these all must exist since
                // `tmpcount` < `valcount`).
                //
                // Now, emit the next value into the last target (the value must exist, since the
                // last step has consumed at most `valcount-1` values): If it is a normal
                // expression (evaluates to a single value), fill all targets after this one with
                // nil.
                //
                // If it is an expression that can evaluate to multiple values (function call,
                // varargs, ...), emit its special opcode. This requires the target slots to be in
                // ascending order, so we may need to allocate more slots. The VM will fill the
                // slots with the result and fill all leftover targets with nil. Then emit MOV ops
                // to assign the temporary slots to their targets.
                //
                // Then, move all temps to their target variables. All targets now have their
                // assigned value.
                //
                // Lastly, emit all leftover values (there may be none, of course) into /dev/null,
                // aka dispose their values (this applies their side effects, which is crucial).
                // TODO implement a lint that warns about assignments with more values than targets

                let varcount = vars.len();
                let valcount = vals.len();
                let tmpcount = cmp::min(varcount-1, valcount-1);
                let tmpstart = self.alloc_slots(tmpcount) as usize;

                if varcount > u8::MAX as usize || valcount > u8::MAX as usize ||
                tmpcount > u8::MAX as usize {
                    self.err("assignment doesn't fit into u8", None);   // XXX
                    return;
                }

                println!("{} tmps", tmpcount);

                for i in 0..tmpcount {
                    let slot: usize = tmpstart + i;
                    if slot > u8::MAX as usize {
                        self.err("slot limit reached", None);
                        return;
                    }

                    let slot = slot as u8;
                    let realslot = self.emit_expr(&vals[i], slot);
                    if realslot != slot {
                        self.emit(MOV(slot, realslot));
                    }
                }

                // Emit next value on rhs. Might return any number of values.
                {
                    let val = &vals[tmpcount];       // in bounds, since tmpcount < valcount
                    let target = &vars[tmpcount];    // in bounds, since tmpcount < varcount

                    let last_value = tmpcount == vals.len() - 1;
                    let vars_left = varcount - tmpcount;
                    if last_value && vars_left > 1 {
                        // might assign multiple results

                        // TODO might be able to optimize the case when locals are the target
                        // (correct order and end of the stack).

                        // Number of results is known at compile-time (`vars_left`). Allocate temp
                        // slots, `emit_expr_multi`, then assign temp slots to targets.
                        let temp_start = self.alloc_slots(vars_left);
                        self.emit_expr_multi(val, temp_start, vars_left as u8);

                        for i in 0..vars_left {
                            self.emit_assign(&vars[tmpcount+i], |_, _| temp_start + i as u8);
                        }
                    } else {
                        // single result, simple assignment. `hint` is allocated by emit_assign.
                        // excessive results will be ignored (`emit_expr` ignores them).
                        self.emit_assign(target, |e, hint| e.emit_expr(val, hint));
                    }
                }

                // move temps to their targets
                for i in 0..tmpcount {
                    let target = &vars[i];
                    self.emit_assign(target, |_, _| i as u8);
                }

                self.dealloc_slots(tmpcount);

                // eval. excess expressions
                for i in tmpcount+1..vals.len() {
                    let slot = self.alloc_slots(1);
                    self.emit_expr(&vals[i], slot);
                    self.dealloc_slots(1);
                }
            }
            _ => panic!("NYI stmt: {:?}", s),    // TODO remove, this is just for testing
        }
    }

    fn visit_expr(&mut self, _e: &Expr) {
        panic!("Emitter::visit_expr entered (this should never happen)");
    }

    fn visit_block(&mut self, b: &'a Block) {
        let old_block = self.block;
        self.block = Some(b);

        // allocate stack slots for all locals in the block
        let oldstack = self.cur_func().stacksize;
        let mut stacksz = self.alloc_slots(b.localmap.len());
        for entry in &b.localmap {
            let (ref name, id) = entry;
            self.alloc.insert(*id, stacksz);
            println!("{} ({}) => {}", id, name, stacksz);
            stacksz += 1;
        }

        walk_block_ref(b, self);

        // deallocate stack slots
        for entry in &b.localmap {
            let (_, id) = entry;
            self.alloc.remove(&id);
        }
        self.cur_func().stacksize = oldstack;

        self.block = old_block;
    }

    fn visit_func(&mut self, f: &'a Function) {
        self.funcs.push(FnData::new(f));

        self.visit_block(&f.body);
        self.emit(RETURN(0, 1));

        let func = self.funcs.pop().unwrap();
        if func.stacksize as u64 > limits::STACK_LIMIT {
            self.err("stack size exceeds maximum value",
                Some(format!("got size {}, max is {}", func.stacksize, limits::STACK_LIMIT)));
            return;
        }

        // TODO shrink vectors to save space

        if self.funcs.is_empty() {
            // just emitted the main function, put it back, we are done
            self.funcs.push(func);
        } else {
            let parent_idx = self.funcs.len()-1;
            let parent = &mut self.funcs[parent_idx];
            parent.child_protos.push(Box::new(func));
        }
    }
}


/// Builds a `FunctionProto` for the given main function and emits byte code for execution by the
/// VM.
pub fn emit_func(f: &Function, source_name: &str) -> EmitResult {
    let mut emitter = Emitter::new(source_name);
    emitter.visit_func(f);

    emitter.get_result()
}


#[cfg(test)]
mod tests {
    use super::*;
    use compiler::parse_and_resolve;
    use program::FnData;

    fn test(code: &str) -> FnData {
        emit_func(&parse_and_resolve(code).unwrap(), "<test>").unwrap()
    }

    #[test]
    fn tdd() {
        test("local i");
    }
}
