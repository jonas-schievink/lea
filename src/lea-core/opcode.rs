//! This module defines the `Opcode` type, which contains all opcodes handled by the VM.

use std::fmt;
use std::ops::{Deref, DerefMut};

pub use self::Opcode::*;

/// An opcode that can be executed by the VM. This enum is always 32 bits large.
///
/// Documentation syntax:
///
/// * `R[n]` is the value stored in register (stack slot) `n`
/// * `U[n]` is the current value of the upvalue with the index `n`
/// * `C[n]` is the constant at index `n` (looked up in the current function's constant table)
/// * `PROTO[n]` is function prototype number `n`
/// * `A`, `B`, `C` are the values of the first, second and third `u8` parameters (resp.)
/// * `Xu` is the value of the second operand, which must be a `u16` (eXtended unsigned)
/// * `Xs` is the value of the second operand, which must be a `i16` (eXtended signed)
/// * `Lu` = `((A << 16) | Xu) as u32` emulates one 24 bit parameter (eg. for jumps)
/// * `Ls` = `((A << 16) | Xu) as i32` signed version of `Lu`
/// * `PC` is the program counter, which holds the number of the next opcode by default
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, RustcEncodable, RustcDecodable)]
pub enum Opcode {
    /// > R[A] := R[B]
    MOV(u8, u8),
    /// > R[A] := C[Xu]
    ///
    /// Loads the constant with index `Xu` into `R[A]`.
    LOADK(u8, u16),
    /// > R[A] := nil, R[A+1] := nil, ..., R[A+B] := nil
    ///
    /// Assigns `nil` to the B+1 registers starting at A. If B is 0, assigns `nil` only to `R[A]`.
    LOADNIL(u8, u8),
    /// > R[A], R[A+1], ..., R[A+B] := C
    ///
    /// Assigns a boolean to B+1 registers from `R[A]` to `R[A+B]`. If C is `false`, assign `false`
    /// to the registers, otherwise assign `true`.
    LOADBOOL(u8, u8, bool),
    /// > R[A] := {}
    ///
    /// Creates a new table and assigns it to `R[A]`.
    TABLE(u8),  // TODO Include initial capacity / Create from template
    /// > R[A] := []
    ///
    /// Creates a new array and assigns it to `R[A]`.
    ARRAY(u8),  // TODO Include initial capacity / Create from template
    /// > R[A] := closure of PROTO[Xu]
    ///
    /// Instantiates function prototype number `Xu` and stores a reference to the created closure
    /// in `R[A]`
    FUNC(u8, u16),
    /// > R[A], R[A+1], ..., R[A+C-2] := R\[A](R[A+1], ..., R[A+B-1])
    ///
    /// Calls `R[A]` with B-1 arguments stored in `R[A+1]` through `R[A+B-1]`. If B is 1, no
    /// arguments are passed. If B is 0, passes all arguments from A+1 to the top of the stack
    /// (this is used if the last argument is a function call, which may return an unknown number
    /// of values).
    ///
    /// Stores C-1 return values in `R[A]` through `R[A+C-2]`. If C is 1, no return values are
    /// stored. If C is 0, stores all return values in `R[A]`, `R[A+1]`, ... (if necessary,
    /// expanding the stack to fit all return values in).
    ///
    /// If the callee returns less than C-1 results, the rest is set to `nil`, except when C is 0,
    /// in which case 0 results may be returned (the top of the stack is set to A in that case).
    ///
    /// This might invoke a metamethod if `R[A]` has a metatable with a `__call` field.
    CALL(u8, u8, u8),
    /// > return R[A], R[A+1], ..., R[A+B-2]
    ///
    /// Returns B-1 values stored in `R[A]` through `R[A+B-2]` to the caller. If B is 1, returns no
    /// values and just leaves the current function, returning control to the caller. If B is 0,
    /// returns all values from A to the top of the stack.
    ///
    /// If the program's main function executes this opcode, the VM will pass the first return
    /// value (if any) to the code that started the VM.
    RETURN(u8, u8),
    /// > close_upval(R[A], R[A+1], ...)
    ///
    /// Closes all open upvalues in registers `A` and higher.
    CLOSE(u8),
    /// > R[A], R[A+1], ..., R[A+B-1] := varargs
    ///
    /// Stores B arguments passed as variable arguments (in `...`) in `R[A]` through `R[A+B-1]`.
    /// If B is 0, stores all varargs in `R[A]`, `R[A+1]`, ... (expanding the stack if necessary).
    VARARGS(u8, u8),
    /// > PC += Xs
    ///
    /// Unconditional jump. Offsets `PC` by `Xs`, which may be negative.
    ///
    /// All jumps share the same property: If `Xs` is 0, PC will not be changed (the jump acts
    /// like a no-op). If `Xs` is -1, an endless loop is created.
    JMP(i16),
    /// > if R[A]: PC += Xs
    ///
    /// Conditional jump. If the value in `R[A]` is "truthy" (not `false` or `nil`), offsets `PC`
    /// by `Xs`.
    IF(u8, i16),
    /// > if not R[A]: PC += Xs
    ///
    /// If the value in `R[A]` is `false` or `nil`, offsets `PC` by `Xs`.
    IFNOT(u8, i16),
    /// > R[A] := U[B]
    GETUPVAL(u8, u8),
    /// > U[A] := R[B]
    SETUPVAL(u8, u8),
    /// > R[A] := R[B][R[C]]
    ///
    /// Indexes `R[B]` with the value in `R[C]` and stores the result in `R[A]`.
    GETIDX(u8, u8, u8),
    /// > R[A][R[B]] := R[C]
    ///
    /// Stores `R[C]` in `R[A]` at index `R[B]`.
    SETIDX(u8, u8, u8),

    // Operator opcodes. Any of them can call a metamethod if the value in R[B] or R[C] has an
    // associated metatable that overrides the operator.

    /// > R[A] := R[B] + R[C]
    ADD(u8, u8, u8),
    /// > R[A] := R[B] - R[C]
    SUB(u8, u8, u8),
    /// > R[A] := R[B] * R[C]
    MUL(u8, u8, u8),
    /// > R[A] := R[B] / R[C]
    DIV(u8, u8, u8),
    /// > R[A] := R[B] % R[C]
    MOD(u8, u8, u8),
    /// > R[A] := R[B] ^ R[C]
    POW(u8, u8, u8),

    /// > R[A] := R[B] == R[C]
    EQ(u8, u8, u8),
    /// > R[A] := R[B] != R[C]
    NEQ(u8, u8, u8),
    /// > R[A] := R[B] <= R[C]
    LEQ(u8, u8, u8),
    /// > R[A] := R[B] >= R[C]
    GEQ(u8, u8, u8),
    /// > R[A] := R[B] < R[C]
    LESS(u8, u8, u8),
    /// > R[A] := R[B] > R[C]
    GREATER(u8, u8, u8),

    // LAND and LOR are implemented with jumps because they can shortcut. This also implies that
    // they cannot be overridden via a metatable.

    /// > R[A] := R[B] & R[C]
    BAND(u8, u8, u8),
    /// > R[A] := R[B] | R[C]
    BOR(u8, u8, u8),
    /// > R[A] := R[B] ~ R[C]
    BXOR(u8, u8, u8),
    /// > R[A] := R[B] << R[C]
    SHIFTL(u8, u8, u8),
    /// > R[A] := R[B] >> R[C]
    SHIFTR(u8, u8, u8),

    /// > R[A] := R[B] .. R[B+1] .. ... .. R[B+C+1]
    ///
    /// Concatenates the values in `R[B]` through `R[B+C+1]`: If C is 0, concatenates `R[B]` and
    /// `R[B+1]`.
    CONCAT(u8, u8, u8),

    /// > R[A] := -R[B]
    NEG(u8, u8),
    /// > R[A] := ~R[B]
    INV(u8, u8),
    /// > R[A] := #R[B]
    LEN(u8, u8),
    /// > R[A] := !R[B]
    ///
    /// The values `false` and `nil` will be converted to `true`, any other value will be
    /// converted to `false`. This does not call a metamethod.
    NOT(u8, u8),
    /// Causes a VM panic
    INVALID,
}

/// Newtype for `Vec<Opcode>` that overrides `Debug` to use at most one line per printed opcode
/// (when the `{:#?}` format specifier is used).
#[derive(Clone, PartialEq, Eq, Hash, RustcEncodable, RustcDecodable)]
pub struct Opcodes(pub Vec<Opcode>);

impl fmt::Debug for Opcodes {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut l = f.debug_list();
        for op in &self.0 {
            l.entry(&SingleLineDebug(op));
        }
        l.finish()
    }
}

impl Deref for Opcodes {
    type Target = Vec<Opcode>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Opcodes {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.0
    }
}

struct SingleLineDebug<'a, T: fmt::Debug + 'a>(&'a T);

impl<'a, T: fmt::Debug + 'a> fmt::Debug for SingleLineDebug<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", &self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::mem;

    #[test]
    fn size() {
        // we guarantee that opcodes fit in 32 bits, regardless of the host platform
        assert_eq!(mem::size_of::<Opcode>(), 4);
    }

    #[test]
    fn format() {
        assert_eq!(format!("\n{:#?}", Opcodes(vec![
            MOV(0, 1),
            IFNOT(5, -1),
            CONCAT(2, 3, 4),
            LOADBOOL(7, 0, true),
        ])), "
[
    MOV(0, 1),
    IFNOT(5, -1),
    CONCAT(2, 3, 4),
    LOADBOOL(7, 0, true)
]");
    }
}
