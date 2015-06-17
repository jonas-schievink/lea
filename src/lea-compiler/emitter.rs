//! This module implements the byte code emitter.

use visit::*;

use ast::*;
use ast::span::{Span, Spanned};
use ast::op::{BinOp, UnOp};

use core::limits;
use core::fndata::FnData;
use core::opcode::*;
use core::literal::*;

use term::{color, Terminal, Attr};

use std::{u8, u16, i16};
use std::collections::HashMap;
use std::cmp;
use std::io::{self, Write};
use std::mem;


#[derive(Clone, Debug)]
pub struct EmitError {
    pub msg: &'static str,
    pub detail: Option<String>,
    pub span: Option<Span>,
}

impl EmitError {
    pub fn format<W: Write>(&self, code: &str, source_name: &str, t: &mut Terminal<Output=W>) -> io::Result<()> {
        let mut msg = self.msg.to_string();
        if let Some(ref detail) = self.detail {
            msg.push_str(&detail);
        }

        match self.span {
            None => {
                try!(t.fg(color::RED));
                try!(write!(t, "error: "));
                try!(t.attr(Attr::Bold));
                try!(t.fg(color::WHITE));
                try!(write!(t, "{}", &msg));
                try!(t.reset());
            }
            Some(span) => {
                try!(span.print_with_err(code, source_name, &msg, t));
            }
        }

        Ok(())
    }
}

/// On success, the main function is returned as an `FnData` object. On error, the `EmitError`s
/// produced are returned.
pub type EmitResult = Result<FnData, Vec<EmitError>>;

struct Emitter {
    source_name: String,
    /// List of errors that have occurred while emitting. They are ignored until the main function
    /// is traversed. If this list isn't empty when the emitter is done, it will not complete the
    /// process and return an error to the caller instead.
    errs: Vec<EmitError>,
    /// Function stack. The last entry is the currently emitted function. When done emitting, the
    /// last `FnData` is popped off and added to the child function list of the parent.
    funcs: Vec<FnData>,

    alloc: HashMap<usize, u8>,
}

impl Emitter {
    fn new(source_name: &str) -> Emitter {
        Emitter {
            source_name: source_name.to_string(),
            errs: Vec::new(),
            funcs: Vec::new(),
            alloc: HashMap::with_capacity(8),
        }
    }

    /// Get the FnData of the currently emitted function
    fn cur_func(&self) -> &FnData {
        let len = self.funcs.len();
        debug_assert!(len > 0);
        &self.funcs[len-1]
    }

    /// Get the FnData of the currently emitted function as a mutable reference
    fn cur_func_mut(&mut self) -> &mut FnData {
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
            self.cur_func_mut().stacksize += count as u8;
            stacksz
        }
    }

    fn dealloc_slots(&mut self, count: usize) {
        debug_assert!(self.cur_func().stacksize as usize >= count);
        self.cur_func_mut().stacksize -= count as u8;
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
            debug!(" CONST {} = {:?}", id, lit);
            self.cur_func_mut().consts.push(lit.clone());
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
            span: None,
        });
    }

    fn err_span(&mut self, msg: &'static str, detail: Option<String>, span: Span) {
        self.errs.push(EmitError {
            msg: msg,
            detail: detail,
            span: Some(span),
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

    /// Emits an opcode into the opcodestream and returns its address/index. Does not apply
    /// optimizations.
    fn emit_raw(&mut self, op: Opcode) -> usize {
        let opcodes = &mut self.cur_func_mut().opcodes;
        let index = opcodes.len();
        opcodes.push(op);

        index
    }

    /// Replaces the opcode at the given index.
    fn replace_op(&mut self, index: usize, new: Opcode) {
        self.cur_func_mut().opcodes[index] = new;
    }

    /// Gets the opcode index ("instruction pointer") after the last opcode that was emitted.
    fn get_next_addr(&self) -> usize {
        self.cur_func().opcodes.len()
    }

    /// Emits an opcode into the current function. Note that this might not add the opcode, but
    /// instead modify the last opcode emitted if a peephole optimization can be applied.
    fn emit(&mut self, op: Opcode) {
        debug!("{:?}", op);

        if self.cur_func().opcodes.len() as u64 >= limits::OP_LIMIT {
            self.err("opcode limit reached", Some(format!("limit: {}", limits::OP_LIMIT)));
        } else {
            /// Applies various simple peephole optimizations. Returns the opcode to replace `last`
            /// with, or `None` if `new` should be added to the opcode list.
            fn peephole_opt(last: Opcode, new: Opcode) -> Option<Opcode> {
                match last {
                    LOADNIL(a, b) => if let LOADNIL(c, d) = new {
                        if c == a + b + 1 {
                            Some(LOADNIL(a, b + d + 1))
                        } else {
                            None
                        }
                    } else { None },
                    LOADK(a, _) => match new {
                        LOADK(b, _) if a == b => Some(new),  // overwrites just-loaded constant
                        LOADNIL(start, dist) if start <= a && a <= start + dist => {
                            // `a` is in the LOADNIL range and will be overwritten
                            Some(new)
                        },
                        _ => None,
                    },
                    _ => None,
                }
            }

            if self.cur_func().opcodes.len() != 0 {
                let len = self.cur_func().opcodes.len();
                let last = self.cur_func_mut().opcodes[len - 1];

                if let Some(new) = peephole_opt(last, op) {
                    mem::replace(&mut self.cur_func_mut().opcodes[len - 1], new);
                    return;
                }
            }

            self.cur_func_mut().opcodes.push(op);
        }
    }

    /// Emits an assignment to a variable. The function `f` is called with an emitter reference and
    /// a slot hint to get the slot the assigned value is located in.
    fn emit_assign<F>(&mut self, target: &Variable, f: F)
    where F: FnOnce(&mut Emitter, u8) -> u8 {
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
                    self.err_span("upvalue limit reached",
                        Some(format!("upvalue #{} over limit {}", id, u8::MAX)), target.span);
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

            VNamed(_) => panic!("VNamed encountered by emitter, resolver is broken"),
        }
    }

    /// Emits a variable used as an expression
    fn emit_var(&mut self, v: &Variable, hint_slot: u8) -> u8 {
        match v.value {
            VLocal(id) => self.get_slot(id),    // ignores hint
            VUpval(id) => {
                if id > u8::MAX as usize {
                    self.err_span("upvalue limit reached",
                        Some(format!("upvalue #{} over limit {}", id, u8::MAX)),
                        v.span);
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
    fn emit_expr_multi(&mut self, e: &Expr, start_slot: u8, max_res: u8) {
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
    fn emit_expr(&mut self, e: &Expr, hint_slot: u8) -> u8 {
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
            EBinOp(ref lhs, op, ref rhs) => {
                match op {
                    BinOp::LAnd | BinOp::LAndLua => {
                        // A && B  <=>  if A then B else A
                        let lslot = self.emit_expr(lhs, hint_slot);
                        let jmp = self.emit_raw(IFNOT(lslot, 0));
                        let rslot = self.emit_expr(rhs, hint_slot);
                        if rslot != hint_slot { self.emit(MOV(hint_slot, rslot)); }

                        // jump here if A is false
                        let rel = self.get_next_addr() - jmp - 1;
                        if rel > i16::MAX as usize {
                            self.err_span("relative jump exceeds limit",
                                Some(format!("jump dist is {}, limit is {}", rel, i16::MAX)),
                                e.span);
                        } else {
                            self.replace_op(jmp, IFNOT(lslot, rel as i16));
                            if lslot != hint_slot { self.emit(MOV(hint_slot, lslot)); }
                        }
                    },
                    BinOp::LOr | BinOp::LOrLua => {
                        // A || B  <=>  if A then A else B
                        let lslot = self.emit_expr(lhs, hint_slot);
                        let jmp = self.emit_raw(IF(lslot, 0));
                        let rslot = self.emit_expr(rhs, hint_slot);
                        if rslot != hint_slot { self.emit(MOV(hint_slot, rslot)); }

                        // jump here if A is true
                        let rel = self.get_next_addr() - jmp - 1;
                        if rel > i16::MAX as usize {
                            self.err_span("relative jump exceeds limit",
                                Some(format!("jump dist is {}, limit is {}", rel, i16::MAX)),
                                e.span);
                        } else {
                            self.replace_op(jmp, IF(lslot, rel as i16));
                            if lslot != hint_slot { self.emit(MOV(hint_slot, lslot)); }
                        }
                    },
                    _ => {
                        // normal bin op. eval lhs and rhs first.
                        let lslotalloc = self.alloc_slots(1);
                        let lslot = self.emit_expr(lhs, lslotalloc);
                        if lslot != lslotalloc { self.dealloc_slots(1); }
                        let rslotalloc = self.alloc_slots(1);
                        let rslot = self.emit_expr(rhs, rslotalloc);
                        if rslot != rslotalloc { self.dealloc_slots(1); }
                        match op {
                            BinOp::Add => self.emit(ADD(hint_slot, lslot, rslot)),
                            BinOp::Sub => self.emit(SUB(hint_slot, lslot, rslot)),
                            BinOp::Mul => self.emit(MUL(hint_slot, lslot, rslot)),
                            BinOp::Div => self.emit(DIV(hint_slot, lslot, rslot)),
                            BinOp::Mod => self.emit(MOD(hint_slot, lslot, rslot)),
                            BinOp::Pow => self.emit(POW(hint_slot, lslot, rslot)),

                            BinOp::Eq => self.emit(EQ(hint_slot, lslot, rslot)),
                            BinOp::NEq | BinOp::NEqLua => self.emit(NEQ(hint_slot, lslot, rslot)),
                            BinOp::LEq => self.emit(LEQ(hint_slot, lslot, rslot)),
                            BinOp::GEq => self.emit(GEQ(hint_slot, lslot, rslot)),
                            BinOp::Less => self.emit(LESS(hint_slot, lslot, rslot)),
                            BinOp::Greater => self.emit(GREATER(hint_slot, lslot, rslot)),

                            BinOp::BAnd => self.emit(BAND(hint_slot, lslot, rslot)),
                            BinOp::BOr => self.emit(BOR(hint_slot, lslot, rslot)),
                            BinOp::BXor => self.emit(BXOR(hint_slot, lslot, rslot)),
                            BinOp::ShiftL => self.emit(SHIFTL(hint_slot, lslot, rslot)),
                            BinOp::ShiftR => self.emit(SHIFTR(hint_slot, lslot, rslot)),

                            BinOp::Concat => {
                                assert_eq!(lslot + 1, rslot);    // TODO ensure this is always true
                                // TODO make use of CONCAT's special behaviour (concat a range of regs)
                                self.emit(CONCAT(hint_slot, lslot, 0));
                            },

                            BinOp::LAnd | BinOp::LAndLua | BinOp::LOr | BinOp::LOrLua => unreachable!(),
                        };

                        if lslot == lslotalloc { self.dealloc_slots(1); }
                        if rslot == rslotalloc { self.dealloc_slots(1); }
                    },
                }

                hint_slot
            },
            EUnOp(op, ref expr) => {
                let slot = self.alloc_slots(1);
                let realslot = self.emit_expr(expr, slot);
                if slot != realslot { self.dealloc_slots(1); }

                match op {
                    UnOp::Negate => self.emit(NEG(hint_slot, realslot)),
                    UnOp::LNot | UnOp::LNotLua => self.emit(NOT(hint_slot, realslot)),
                    UnOp::BNot => self.emit(INV(hint_slot, realslot)),
                    UnOp::Len => self.emit(LEN(hint_slot, realslot)),
                }

                if slot == realslot { self.dealloc_slots(1); }
                hint_slot
            },
            EBraced(ref e) => {
                self.emit_expr(&**e, hint_slot)
            },
            ERawOp(..) => panic!("ERawOp encountered by emitter"),
            _ => panic!("NYI expression {:?}", e),  // TODO remove
        }
    }

    fn emit_stmt(&mut self, s: &Stmt, block: &Block) {
        match s.value {
            SDecl(ref names, ref exprs) => {
                // allocate stack slots for locals
                let mut locals = Vec::<Variable>::new();
                for name in names {
                    let id = *block.get_local(name).unwrap();
                    let slot = self.alloc_slots(1);
                    self.alloc.insert(id, slot);
                    locals.push(Spanned::default(VLocal(id)));

                    debug!("alloc: {} (id {}) -> slot {}", name.value, id, slot);
                }

                // build fake assignment node and use generic assigment code to emit code
                let mut vals = exprs.clone();
                vals.push(Spanned::default(ELit(TNil)));

                let assign = SAssign(locals, vals);
                self.emit_stmt(&Spanned::new(s.span, assign), block);
            }
            SAssign(ref vars, ref vals) => {
                // We need `min(varcount-1, valcount-1)` temps (= `tmpcount`). Eval first
                // `tmpcount` expressions on the right-hand side into the temps (these all must
                // exist since `tmpcount` < `valcount`).
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
                    self.err_span("assignment doesn't fit into u8", None, s.span);   // XXX
                    return;
                }

                debug!("assign: {} tmps", tmpcount);

                for i in 0..tmpcount {
                    let slot: usize = tmpstart + i;
                    if slot > u8::MAX as usize {
                        self.err_span("slot limit reached", None, s.span);
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
                    if last_value && vars_left > 1 && val.is_multi_result() {
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

                        for i in 1..vars_left {
                            self.emit_assign(&vars[tmpcount+i], |e, hint| {
                                e.emit(LOADNIL(hint, 0));
                                hint
                            });
                        }
                    }
                }

                // move temps to their targets
                for i in 0..tmpcount {
                    let target = &vars[i];
                    self.emit_assign(target, |_, _| (tmpstart + i) as u8);
                }

                self.dealloc_slots(tmpcount);

                // eval. excess expressions (if they could cause side effects)
                for i in tmpcount+1..vals.len() {
                    let expr = &vals[i];
                    if expr.has_side_effects() {
                        let slot = self.alloc_slots(1);
                        self.emit_expr(expr, slot);
                        self.dealloc_slots(1);
                    }
                }
            }
            _ => panic!("NYI stmt: {:?}", s),    // TODO remove, this is just for testing
        }
    }
}

impl <'a> Visitor<'a> for Emitter {
    fn visit_stmt(&mut self, _: &Stmt) {
        panic!("Emitter::visit_stmt entered (this should never happen)");
    }

    fn visit_expr(&mut self, _: &Expr) {
        panic!("Emitter::visit_expr entered (this should never happen)");
    }

    fn visit_block(&mut self, b: &Block) {
        let oldstack = self.cur_func().stacksize;
        for stmt in &b.stmts {
            self.emit_stmt(stmt, b);
        }

        // deallocate stack slots
        for entry in &b.localmap {
            let (_, id) = entry;
            self.alloc.remove(&id);
        }
        self.cur_func_mut().stacksize = oldstack;
    }

    fn visit_func(&mut self, f: &Function) {
        self.funcs.push(FnData {
            stacksize: 0,
            params: f.params.len(),
            varargs: f.varargs,
            opcodes: vec![],
            consts: vec![],
            upvals: f.upvalues.clone(),
            lines: vec![],
            source_name: self.source_name.clone(),
            child_protos: vec![],
        });

        self.visit_block(&f.body);
        self.emit(RETURN(0, 1));

        let func = self.funcs.pop().unwrap();
        if func.stacksize as u64 > limits::STACK_LIMIT {
            self.err_span("stack size exceeds maximum value",
                Some(format!("got size {}, max is {}", func.stacksize, limits::STACK_LIMIT)),
                f.body.span);
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


/// Builds a `FnData` structure for the given main function and emits byte code for execution by
/// the VM.
pub fn emit_func(f: &Function, source_name: &str) -> EmitResult {
    let mut emitter = Emitter::new(source_name);
    emitter.visit_func(f);

    emitter.get_result()
}


#[cfg(test)]
mod tests {
    use super::*;
    use ::parse_and_resolve;

    use core::opcode::*;

    /// A simple test that compiles a main function and compares the emitted opcodes
    macro_rules! test {
        ($code:expr => [ $($op:expr,)* ]) => {{
            let opvec = emit_func(&parse_and_resolve($code).unwrap(), "<test>").unwrap().opcodes;

            assert_eq!(opvec, vec![ $($op),* ]);
        }}
    }

    #[test]
    fn assign_simple() {
        test!("local i, j    i, j = j, i, 0" => [
            LOADNIL(0,1),
            MOV(2,1),
            MOV(1,0),
            MOV(0,2),
            RETURN(0,1),
        ]);
        test!("local i, j    i, j = i, j" => [
            LOADNIL(0,1),
            MOV(2,0),
            MOV(0,2),
            RETURN(0,1),
        ]);
        test!("local i = 0, 1, 2" => [
            LOADK(0,0),
            RETURN(0,1),
        ]);
        test!("local i, j = true, false" => [
            LOADBOOL(2,0,true),
            LOADBOOL(1,0,false),
            MOV(0,2),
            RETURN(0,1),
        ]);
        test!("local i, j    i, j = j" => [
            LOADNIL(0,1),
            MOV(0,1),
            LOADNIL(1,0),
            RETURN(0,1),
        ]);
    }

    #[test]
    fn assign_multi() {
        test!("local i, j   i, j = ..." => [
            LOADNIL(0,1),
            VARARGS(2,2),   // slots 2 and 3
            MOV(0,2),       // TODO the MOV isn't necessary, since i and j are in order
            MOV(1,3),
            RETURN(0,1),
        ]);
    }

    #[test]
    fn binops() {
        test!("local i, j  i = i + j" => [
            LOADNIL(0,1),
            ADD(0,0,1),
            RETURN(0,1),
        ]);
        test!("local i, j  i = i + j - i * i / i" => [
            LOADNIL(0,1),
            ADD(2,0,1),
            MUL(4,0,0),     // TODO unnecessary temp reg
            DIV(3,4,0),
            SUB(0,2,3),
            RETURN(0,1),
        ]);
        test!("local i, j  i = i == j > j" => [
            LOADNIL(0,1),
            EQ(2,0,1),
            GREATER(0,2,1),
            RETURN(0,1),
        ]);
    }

    #[test]
    fn unops() {
        test!("local i  i = -#i" => [
            LOADNIL(0,0),
            LEN(1,0),
            NEG(0,1),
            RETURN(0,1),
        ]);
    }

    #[test]
    fn shortcut() {
        test!("local i, j, k   i = i && j || k" => [
            LOADNIL(0,2),
            IFNOT(0,1),     // -> [1]
            MOV(0,1),       // i := j (since i is truthy)
            IF(0,1),        //[1] -> [2] TODO this jump is always taken
            MOV(0,2),       // i := k (since i is false)
            RETURN(0,1),    //[2]
        ]);
    }
}
