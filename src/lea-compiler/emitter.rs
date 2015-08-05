//! This module implements the byte code emitter.

use ast::*;
use ast::visit::*;
use parser::op::{BinOp, UnOp};
use parser::span::{Span, Spanned};

use lea_core::limits;
use lea_core::fndata::{UpvalDesc, FnData};
use lea_core::opcode::*;
use lea_core::constant::Const;

use term::{color, Terminal, Attr};

use std::{u8, u16, i16};
use std::collections::HashMap;
use std::cmp;
use std::io::{self, Write};
use std::mem;

// TODO: fn build_jump_to(&mut self, target: usize) { ... }

#[derive(Clone, Debug)]
pub struct EmitError {
    pub msg: &'static str,
    pub detail: Option<String>,
    pub span: Option<Span>,
}

impl EmitError {
    pub fn format<W: Write>(&self, code: &str, source_name: &str, t: &mut Terminal<Output=W>) -> io::Result<()> {
        let msg = match self.detail {
            Some(ref detail) => format!("{} ({})", self.msg, detail),
            None => self.msg.to_string(),
        };

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

    /// Maps all currently reachable locals to their associated stack slots.
    alloc: HashMap<usize, u8>,

    /// Set to `true` when emitting a `FUNC` opcode with a function that references a local
    /// declared in the current block as an upvalue. Causes the emission of a `CLOSE` opcode when
    /// leaving the current block.
    needs_close: bool,

    /// Opcode indices of `SBreak` (was emitted as opcode `INVALID`). These are replaced by
    /// forward jumps after the current loop when the loop is fully emitted.
    break_indices: Vec<usize>,
}

impl Emitter {
    fn new(source_name: &str) -> Emitter {
        Emitter {
            source_name: source_name.to_string(),
            errs: Vec::new(),
            funcs: Vec::new(),
            alloc: HashMap::new(),
            needs_close: false,
            break_indices: Vec::new(),
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

    /// Gets the slot the given local is allocated into. Panics if the local isn't reachable in
    /// the current scope.
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
    fn add_const(&mut self, c: &Const) -> u16 {
        // ensure that only "useful" constant are added
        debug_assert!(match *c {
            Const::Int(..) | Const::Float(..) | Const::Str(..) => true,
            Const::Bool(..) | Const::Nil => false, // handled by special opcodes
        });

        {
            let func = self.cur_func();
            for i in 0..func.consts.len() {
                let c2 = &func.consts[i];
                if c == c2 {
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
            debug!(" CONST {} = {:?}", id, c);
            self.cur_func_mut().consts.push(c.clone());
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

    /// Emits an opcode into the opcode stream and returns its address/index. Does not apply
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

    /// Insert forward jumps to the next opcode for all `break` statements that were encountered
    /// since this method was last called.
    ///
    /// This must be called after emitting a loop.
    fn finalize_breaks(&mut self) {
        let mut break_indices = mem::replace(&mut self.break_indices, Vec::new());
        for &index in &break_indices {
            let rel = self.get_next_addr() - index - 1;
            if rel > i16::MAX as usize {
                self.err("relative jump exceeds limit (when emitting break statement)",
                    Some(format!("jump dist is {}, limit is {}", rel, i16::MAX)));
            }
            self.replace_op(index, JMP(rel as i16));
        }
        break_indices.clear();

        mem::replace(&mut self.break_indices, break_indices);
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

            VNamed(_) => panic!("VNamed encountered by emitter, resolver is broken")
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
            }
            VIndex(ref var, ref idx) => {
                let var_slot = self.alloc_slots(1);
                let var_slot = self.emit_var(&*var, var_slot);
                let idx_slot = self.alloc_slots(1);
                let idx_slot = self.emit_expr(&*idx, idx_slot);

                self.emit(GETIDX(hint_slot, var_slot, idx_slot));

                self.dealloc_slots(2);
                hint_slot
            }

            VNamed(_) => panic!("VNamed encountered by emitter, resolver is broken")
        }
    }

    /// Emits a list of expressions. This will evaluate all expressions from left to right. The
    /// last expression may evaluate to any number of results.
    ///
    /// The results will be stored in the next `n` stack slots.
    ///
    /// The passed closure will be called when all expressions are emitted, with the total number
    /// of arguments (slots) plus 1. If the last argument to the closure (count) is 0, the last
    /// expression can evaluate to any number of values at runtime, which will be placed after the
    /// last fixed argument.
    fn emit_expr_list<'a, F>(&mut self, exprs: &[Expr<'a>], f: F)
    where F: FnOnce(&mut Emitter, /* count-1 */ u8) {
        if exprs.len() == 0 {
            f(self, 1);
            return
        }

        if exprs.len() > 254 {
            self.err_span(
                "too many call arguments",
                Some(format!("got {} arguments, limit is {}", exprs.len(), 254)),
                exprs[exprs.len() - 1].span
            );
        }

        // emit all args except the last one into freshly allocated registers (these must
        // be allocated in ascending order)
        for i in 0..exprs.len()-1 {
            let arg = &exprs[i];
            let slot = self.alloc_slots(1);
            self.emit_expr_into(arg, slot);
        }

        // emit last expression
        let last_arg = &exprs[exprs.len() - 1];
        if last_arg.is_multi_result() {
            // `dummy` isn't a closure because that causes monomorphization to never terminate
            fn dummy(_: &mut Emitter, _: u8) {}
            self.emit_expr_multi(last_arg, 0, dummy);
        } else {
            let slot = self.alloc_slots(1);
            self.emit_expr_into(last_arg, slot);
        }

        f(self, if last_arg.is_multi_result() { 0 } else { exprs.len() as u8 + 1 });

        // in the process, we've allocated `argv.len()-1` slots if the last arg is variable
        // and `argv.len()` slots if not.
        if last_arg.is_multi_result() {
            self.dealloc_slots(exprs.len() - 1);
        } else {
            self.dealloc_slots(exprs.len());
        }
    }

    /// Emits a call.
    ///
    /// First, the callee is evaluated and put in a register. Then, all arguments are evaluated
    /// from left to right and put into the registers after the callee. The last argument might be
    /// an expression returning multiple values, in which case the generated `CALL` instruction
    /// will pass all values until the top of the stack to the callee.
    ///
    /// After the `CALL` instruction is executed, up to `max_res-1` results will be preserved in
    /// the registers that originally contained the callee and call arguments. If `max_res` is 0,
    /// all results will be preserved (the VM updates the top of the stack accordingly).
    ///
    /// The passed closure is called with the first register that contains a returned value and
    /// should be used to process the call results, since the stack slots are deallocated
    /// after calling it (except when `max_res` is 0).
    ///
    /// There are exactly `max_res-1` valid results following the start slot, except when `max_res`
    /// is 0, in which case there are between 0 (so the start slot isn't valid either), and an
    /// implementation-defined limit.
    fn emit_call<F>(&mut self, c: &Call, max_res: u8, f: F) where F: FnOnce(&mut Emitter, u8) {
        match *c {
            SimpleCall(ref callee, ref args) => {
                // Regular call: f(e1, e2, ..)
                // Stack: FUNCTION | ARGS...

                let func_slot = self.alloc_slots(1);
                self.emit_expr_into(callee, func_slot);

                self.emit_expr_list(args, |emitter, count| {
                    // `CALL`s semantics match exactly
                    emitter.emit(CALL(func_slot, count, max_res));

                    // ret values stored starting at A (callee slot)
                    f(emitter, func_slot);
                });

                self.dealloc_slots(1);  // func_slot
            }
            MethodCall(ref obj, ref name, ref args) => {
                // some.thing:name(...) - passes `some.thing` as the first argument, without
                // evaluating it twice (unlike `some.thing.name(some.thing, ...)`).

                // Stack: METHOD | OBJECT | ARGS...

                let method_slot = self.alloc_slots(1);
                let obj_slot = self.alloc_slots(1);
                self.emit_expr_into(obj, obj_slot);

                let const_id = self.add_const(&Const::Str(name.to_string()));

                self.emit(LOADK(method_slot, const_id));
                self.emit(GETIDX(method_slot, obj_slot, method_slot));

                self.emit_expr_list(args, |emitter, count| {
                    // we actually pass one more argument than `emit_expr_list` tells us: the
                    // object.
                    if count == 0 {
                        // dynamic arg count
                        emitter.emit(CALL(method_slot, 0, max_res));
                    } else {
                        // fixed count of `count - 1` arguments, `CALL` passes `B-1` arguments as
                        // well, so we still need to add 1 to pass the object.
                        emitter.emit(CALL(method_slot, count + 1, max_res));
                    }

                    f(emitter, method_slot);
                });

                self.dealloc_slots(2);  // method_slot, obj_slot
            }
        }
    }

    /// Emits an expression that can return multiple values. Up to `max_res` values are retained.
    /// If `max_res` is 0, all results are retained.
    ///
    /// The given closure will be called with the first slot that contains a value created by this
    /// expression. If `max_res` is 0, this slot might not be valid. Otherwise, exactly `max_res`
    /// slots have a valid result in them.
    fn emit_expr_multi<F>(&mut self, e: &Expr, max_res: u8, f: F)
    where F: FnOnce(&mut Emitter, /* start */ u8) {
        match e.value {
            ECall(ref call) => {
                self.emit_call(call, max_res, f);
            }
            EVarArgs => {
                let start_slot = if max_res == 0 {
                    // top of stack
                    self.cur_func().stacksize
                } else {
                    // alloc fixed reg count
                    self.alloc_slots(max_res as usize)
                };

                self.emit(VARARGS(start_slot, max_res));

                f(self, start_slot);

                if max_res != 0 {
                    self.dealloc_slots(max_res as usize);
                }
            }
            _ => {
                debug_assert_eq!(e.is_multi_result(), false);

                // These all have a single result
                let target = self.alloc_slots(cmp::max(1, max_res) as usize);
                self.emit_expr_into(e, target);

                if max_res > 1 {
                    self.emit(LOADNIL(target+1, max_res-2));
                }

                f(self, target);

                self.dealloc_slots(cmp::max(1, max_res) as usize);
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
            ECall(ref call) => {
                self.emit_call(call, 2, |emitter, slot| {
                    // need to move, since `slot` is deallocated
                    emitter.emit(MOV(hint_slot, slot));
                });

                hint_slot
            }
            ELit(ref lit) => {
                match *lit {
                    Const::Nil => {
                        self.emit(LOADNIL(hint_slot, 0));
                        hint_slot
                    }
                    Const::Int(_) | Const::Float(_) | Const::Str(_) => {
                        let id = self.add_const(lit);
                        self.emit(LOADK(hint_slot, id));
                        hint_slot
                    }
                    Const::Bool(b) => {
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
                        let jmp = self.emit_raw(INVALID);
                        self.emit_expr_into(rhs, hint_slot);

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
                    }
                    BinOp::LOr | BinOp::LOrLua => {
                        // A || B  <=>  if A then A else B
                        let lslot = self.emit_expr(lhs, hint_slot);
                        let jmp = self.emit_raw(INVALID);
                        self.emit_expr_into(rhs, hint_slot);

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
                    }
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
                    }
                }

                hint_slot
            }
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
            }
            EFunc(ref func) => {
                let id = self.emit_func(func);
                self.emit(FUNC(hint_slot, id as u16));
                hint_slot
            }
            EVarArgs => {
                // Fetch the first vararg into `hint_slot`
                self.emit(VARARGS(hint_slot, 1));
                hint_slot
            }
            ETable(ref _cons) => {
                // TODO Use table prototypes
                unimplemented!();
            }
            EArray(ref elems) => {
                // TODO Use array prototypes

                // Create new array and put all elements into consecutive indexes
                self.emit(ARRAY(hint_slot));
                let idx_slot = self.alloc_slots(1);
                let tmp = self.alloc_slots(1);
                for (i, elem) in elems.iter().enumerate() {
                    let real = self.emit_expr(elem, tmp);
                    let constid = self.add_const(&Const::Int(i as i64));

                    // TODO Specialized `SETIDX` for constant strings/integers
                    self.emit(LOADK(idx_slot, constid));
                    self.emit(SETIDX(hint_slot, idx_slot, real));
                }
                self.dealloc_slots(1);
                self.dealloc_slots(1);
                hint_slot
            }
        }
    }

    /// Emits an expression into the given slot.
    fn emit_expr_into(&mut self, e: &Expr, slot: u8) {
        let real_slot = self.emit_expr(e, slot);
        if real_slot != slot {
            self.emit(MOV(slot, real_slot));
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
                vals.push(Spanned::default(ELit(Const::Nil)));

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
                    self.emit_expr_into(&vals[i], slot);
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
                        self.emit_expr_multi(val, vars_left as u8, |emitter, temp_start| {
                            for i in 0..vars_left {
                                emitter.emit_assign(&vars[tmpcount+i], |_, _| temp_start + i as u8);
                            }
                        });
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
                let slot = self.alloc_slots(1);
                for i in tmpcount+1..vals.len() {
                    let expr = &vals[i];
                    if expr.has_side_effects() {
                        self.emit_expr(expr, slot);
                    }
                }
                self.dealloc_slots(1);
            }
            SDo(ref block) => {
                self.visit_block(block);
            }
            SBreak => {
                let index = self.emit_raw(INVALID);
                self.break_indices.push(index);
            }
            SReturn(ref vals) => {
                let start = self.cur_func().stacksize;  // first return value here (if any)
                self.emit_expr_list(vals, |emitter, count| {
                    emitter.emit(RETURN(start, count));
                });
            }
            SCall(ref call) => {
                self.emit_call(call, 1, |_, _| ());    // store 0 results
            }
            SIf { ref cond, ref body, ref el } => {
                let cond_slot = self.alloc_slots(1);
                let cond_slot = self.emit_expr(cond, cond_slot);

                // If not cond, jump to else. Emit dummy opcode, will be replaced by IFNOT
                let jump_op = self.emit_raw(INVALID);

                self.dealloc_slots(1);  // cond_slot no longer needed

                self.visit_block(body);

                let rel = self.get_next_addr() - jump_op - 1;
                if rel > i16::MAX as usize {
                    self.err_span("relative jump exceeds limit",
                        Some(format!("jump dist is {}, limit is {}", rel, i16::MAX)),
                        s.span);
                }
                self.replace_op(jump_op, IFNOT(cond_slot, rel as i16));

                if let Some(ref el) = *el {
                    self.visit_block(el);
                }
            }
            SWhile { ref cond, ref body } => {
                let cond_opcode = self.get_next_addr() as isize; // start of cond eval
                let cond_slot = self.alloc_slots(1);
                let cond_slot = self.emit_expr(cond, cond_slot);

                // If not cond, jump behind body
                let jump_op = self.emit_raw(INVALID);

                self.dealloc_slots(1);  // cond_slot no longer needed

                self.visit_block(body);

                // Emit backwards jump
                let rel = cond_opcode - self.get_next_addr() as isize - 1;
                if rel > i16::MAX as isize || rel < i16::MIN as isize {
                    self.err_span("relative jump exceeds limit",
                        Some(format!("jump dist is {}, limit is {}", rel, i16::MAX)),
                        s.span);
                }
                self.emit(JMP(rel as i16));

                let rel = self.get_next_addr() - jump_op - 1;
                if rel > i16::MAX as usize {
                    self.err_span("relative jump exceeds limit",
                        Some(format!("jump dist is {}, limit is {}", rel, i16::MAX)),
                        s.span);
                }
                self.replace_op(jump_op, IFNOT(cond_slot, rel as i16));
                self.finalize_breaks();
            }
            SRepeat { ref body, ref abort_on } => {
                let head_op = self.get_next_addr() as isize;

                self.visit_block(body);

                let abort_cond_slot = self.alloc_slots(1);
                let abort_cond_slot = self.emit_expr(abort_on, abort_cond_slot);

                // If not abort_on, jump to `head_op`
                let rel = head_op - self.get_next_addr() as isize - 1;
                if rel > i16::MAX as isize || rel < i16::MIN as isize {
                    self.err_span("relative jump exceeds limit",
                        Some(format!("jump dist is {}, limit is {}", rel, i16::MAX)),
                        s.span);
                }
                self.emit(IFNOT(abort_cond_slot, rel as i16));

                self.dealloc_slots(1);  // cond_slot
                self.finalize_breaks();
            }
            /*SFor { ref var, ref start, ref step, ref end, ref body } => {
                unimplemented!();
                self.finalize_breaks();
            }
            SForIn { ref vars, ref iter, ref body } => {
                unimplemented!();
                self.finalize_breaks();
            }*/

            _ => panic!("NYI stmt: {:?}", s),    // TODO remove, this is just for testing
        }
    }

    /// Emits a function and returns its ID
    fn emit_func(&mut self, f: &Function) -> u16 {
        if f.params.len() > u8::MAX as usize {
            self.err_span("too many function parameters", Some(format!("function has {}, maximum is {}", f.params.len(), u8::MAX)), f.params[f.params.len() - 1].span);
            return 0
        }

        self.funcs.push(FnData {
            stacksize: 0,
            params: f.params.len() as u8,
            varargs: f.varargs,
            opcodes: Opcodes(vec![]),
            consts: vec![],
            upvals: f.upvalues.clone(),     // TODO Convert
            lines: vec![],
            source_name: self.source_name.clone(),
            child_protos: vec![],
        });

        self.visit_block(&f.body);
        self.emit(RETURN(0, 1));

        let mut func = self.funcs.pop().unwrap();
        if func.stacksize as u64 > limits::STACK_LIMIT {
            self.err_span("stack size exceeds maximum value",
                Some(format!("got size {}, max is {}", func.stacksize, limits::STACK_LIMIT)),
                f.body.span);
            return 0
        }

        // Need to emit a CLOSE in the containing block?
        if f.upvalues.iter().find(|upval| match **upval {
            // TODO only true if the local ID is in the outermost block
            UpvalDesc::Local(_) => true,
            _ => false,
        }).is_some() {
            self.needs_close = true;
        }

        // Convert upvalue descriptions
        func.upvals = f.upvalues.iter().cloned().map(|upval| match upval {
            UpvalDesc::Local(id) => {
                // translate to stack slot
                UpvalDesc::Stack(*self.alloc.get(&id).expect(
                    &format!("local #{} not in alloc map when converting stack slots", id)
                ))
            }
            UpvalDesc::Stack(_) => unreachable!(),
            UpvalDesc::Upval(id) => UpvalDesc::Upval(id),
        }).collect();

        // TODO shrink vectors to save space

        if self.funcs.is_empty() {
            // just emitted the main function, put it back, we are done
            self.funcs.push(func);
            return 0
        } else {
            let id;
            {
                let parent_idx = self.funcs.len()-1;
                let parent = &mut self.funcs[parent_idx];
                id = parent.child_protos.len();
                parent.child_protos.push(Box::new(func));
            }

            if id > u16::MAX as usize {
                self.err_span(
                    "function limit reached",
                    Some(format!("limit is {}", u16::MAX)),
                    f.body.span
                );
                return 0
            }

            id as u16
        }
    }
}

impl<'a> Visitor<'a> for Emitter {
    fn visit_stmt(&mut self, _: &Stmt) {
        panic!("Emitter::visit_stmt entered (this should never happen)");
    }

    fn visit_expr(&mut self, _: &Expr) {
        panic!("Emitter::visit_expr entered (this should never happen)");
    }

    fn visit_block(&mut self, b: &Block) {
        let oldstack = self.cur_func().stacksize;
        let old_needs_close = self.needs_close;
        self.needs_close = false;

        for stmt in &b.stmts {
            self.emit_stmt(stmt, b);
        }

        if self.needs_close {
            self.emit(CLOSE(oldstack));
        }

        // deallocate stack slots
        for entry in &b.localmap {
            let (name, id) = entry;
            let slot = self.alloc.remove(&id)
                .expect(&format!("local {} ({}) wasn't in alloc map", name, id));

            debug!("dealloc: {} (id {}) -> slot {}", name, id, slot);
        }

        self.cur_func_mut().stacksize = oldstack;
        self.needs_close = old_needs_close;
    }

    fn visit_func(&mut self, f: &Function) {
        self.emit_func(f);
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

    use lea_core::opcode::*;

    /// A simple test that compiles a main function and compares the emitted opcodes
    macro_rules! test {
        ($code:expr => [ $($op:expr,)* ]) => {{
            let opvec = emit_func(&parse_and_resolve($code).unwrap(), "<test>").unwrap().opcodes;

            assert_eq!(*opvec, vec![ $($op),* ]);
        }}
    }

    #[test] #[should_panic]
    fn metatest1() {
        // Test that the `test!` macro works
        test!("blergh§6§25&78%&)" => [
            MOV(0,0),
        ]);
    }

    #[test] #[should_panic]
    fn metatest2() {
        // Test that the `test!` macro works
        test!("function fn() end print()" => [
            MOV(12,34),
        ]);
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

    #[test]
    fn scope() {
        test!("local i do local j = i i = j end do local j = i end" => [
            LOADNIL(0,0),   // local i
            MOV(1,0),       // local j = i
            MOV(0,1),       // i = j
            MOV(1,0),       // local j = i
            RETURN(0,1),
        ]);
    }

    #[test]
    fn call() {
        test!("local f   f()  f()  f(nil)  f(nil, nil)   f(f)" => [
            LOADNIL(0,0),   // local f
            MOV(1,0),       // callee: f
            CALL(1,1,1),
            MOV(1,0),       // callee: f
            CALL(1,1,1),
            MOV(1,0),       // callee: f
            LOADNIL(2,0),   // args: nil
            CALL(1,2,1),
            MOV(1,0),       // callee: f
            LOADNIL(2,1),   // args: nil, nil
            CALL(1,3,1),
            MOV(1,0),       // callee: f
            MOV(2,0),       // args: f
            CALL(1,2,1),
            RETURN(0,1),
        ]);
        test!("local f  f('a')  f 'a'" => [
            LOADNIL(0,0),   // local f
            MOV(1,0),       // callee: f
            LOADK(2,0),     // args: 'a'
            CALL(1,2,1),
            MOV(1,0),       // callee: f
            LOADK(2,0),     // args: 'a'
            CALL(1,2,1),
            RETURN(0,1),
        ]);
        test!("local f  f(...)  f(f())" => [
            LOADNIL(0,0),   // local f
            MOV(1,0),       // callee: f
            VARARGS(2,0),   // args: ...
            CALL(1,0,1),
            MOV(1,0),       // 0 callee: f
            MOV(2,0),       // 1 callee: f
            CALL(2,1,0),    // arg: f() (multi expr)
            CALL(1,0,1),    // 0 call: f(f())
            RETURN(0,1),
        ]);
        test!("local f  f(f(), nil)" => [
            LOADNIL(0,0),   // local f
            MOV(1,0),       // 0 callee: f
            MOV(3,0),       // 1 callee: f      TODO we use unnecessary temp reg 3 here
            CALL(3,1,2),    // arg: f()
            MOV(2,3),
            LOADNIL(3,0),   // arg: nil
            CALL(1,3,1),    // 0 call: f(f(), nil)
            RETURN(0,1),
        ]);
        test!("local f  f(nil, f())" => [
            LOADNIL(0,0),   // local f
            MOV(1,0),       // 0 callee: f
            LOADNIL(2,0),   // arg: nil
            MOV(3,0),       // 1 callee: f
            CALL(3,1,0),    // arg: f()
            CALL(1,0,1),    // 0 call: f(nil, f())
            RETURN(0,1),
        ]);
    }

    #[test]
    fn methodcall() {
        test!("local o  o:m(o:n())" => [
            LOADNIL(0,0),   // local o
            // o:m(...)
            // Stack: 1=o.m 2=o
            MOV(2,0),       // o
            LOADK(1,0),     // "m"
            GETIDX(1,2,1),  // o.m
            // o:n()
            // Stack: 3=o.n 4=o
            MOV(4,0),       // o
            LOADK(3,1),     // "n"
            GETIDX(3,4,3),  // o.n
            CALL(3,2,0),
            CALL(1,0,1),
            RETURN(0,1),
        ]);
    }

    #[test]
    fn ret() {
        test!("return" => [
            RETURN(0,1),
            RETURN(0,1),
        ]);
        test!("return true" => [
            LOADBOOL(0, 0, true),
            RETURN(0,2),
            RETURN(0,1),
        ]);
        test!("return ..." => [
            VARARGS(0,0),
            RETURN(0,0),
            RETURN(0,1),
        ]);
        test!("return ..., true" => [
            VARARGS(0,1),
            LOADBOOL(1,0,true),
            RETURN(0,3),
            RETURN(0,1),
        ]);
    }

    #[test]
    fn function() {
        test!("local f  f = function() end    local function g() end" => [
            LOADNIL(0,0),   // local f
            FUNC(0,0),
            LOADNIL(1,0),   // local g
            FUNC(1,1),
            RETURN(0,1),
        ]);
    }

    #[test]
    fn array() {
        test!("local a = []   local b = [nil,true,false]" => [
            ARRAY(0),       // a
            ARRAY(1),       // b
            LOADNIL(3,0),
            LOADK(2,0),     // 0
            SETIDX(1,2,3),
            LOADBOOL(3,0,true),
            LOADK(2,1),     // 1
            SETIDX(1,2,3),
            LOADBOOL(3,0,false),
            LOADK(2,2),     // 2
            SETIDX(1,2,3),
            RETURN(0,1),
        ]);
    }

    #[test]
    fn upvalues() {
        test!("i = nil" => [
            // _ENV.i = nil
            GETUPVAL(0,0),
            LOADK(1,0),     // "i"
            LOADNIL(2,0),
            SETIDX(0,1,2),
            RETURN(0,1),
        ]);
        test!("local function f() f = nil end" => [
            LOADNIL(0,0),   // local f
            FUNC(0,0),
            CLOSE(0),
            RETURN(0,1),
        ]);
        test!("local f, g function f() g = nil end" => [
            LOADNIL(0,1),   // local f, g
            FUNC(0,0),
            CLOSE(0),       // TODO Closes f and g, but only g is needed
            RETURN(0,1),
        ]);
        test!("local function f() end" => [
            LOADNIL(0,0),   // local f
            FUNC(0,0),
            // No close, since no upvalues of type `Local`
            RETURN(0,1),
        ]);
    }

    #[test]
    fn if_then_else() {
        // TODO optimize unconditional if
        test!("if nil then else end" => [
            LOADNIL(0,0),
            IFNOT(0,0),
            RETURN(0,1),
        ]);
        test!("if false then local i else local j end" => [
            LOADBOOL(0,0,false),
            IFNOT(0,1),
            LOADNIL(0,0),   // local i
            LOADNIL(0,0),   // local j
            RETURN(0,1),
        ]);
    }

    #[test]
    fn while_do() {
        // TODO optimize unconditional loops
        test!("while true do end" => [
            LOADBOOL(0,0,true),
            IFNOT(0,1),
            JMP(-3),
            RETURN(0,1),
        ]);
        test!("local i  while i do i = i + 1 end" => [
            LOADNIL(0,0),   // local i
            IFNOT(0,3),
            LOADK(1,0),     // 1
            ADD(0,0,1),     // i = i + 1
            JMP(-4),
            RETURN(0,1),
        ]);
    }

    #[test]
    fn repeat_until() {
        test!("repeat until nil" => [
            LOADNIL(0,0),
            IFNOT(0,-2),
            RETURN(0,1),
        ]);
        test!("local i   repeat i = i + 1 until i" => [
            LOADNIL(0,0),   // local i
            LOADK(1,0),     // 1
            ADD(0,0,1),     // i = i + 1
            IFNOT(0,-3),
            RETURN(0,1),
        ]);
    }

    #[test]
    fn break_loop() {
        test!("repeat break break until nil" => [
            JMP(3),
            JMP(2),
            LOADNIL(0,0),
            IFNOT(0,-4),
            RETURN(0,1),
        ]);
    }
}
