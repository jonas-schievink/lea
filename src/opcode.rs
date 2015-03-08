//! This module defines the `Opcode` struct and all opcodes handled by the VM.


/// An opcode that is executed by the VM.
///
/// Documentation syntax:
/// * `R[n]` is the value stored in register (stack slot) `n`
/// * `C[n]` is the constant at index `n` (looked up in the current function's constant table)
/// * `A`, `B`, `C` are the values of the first, second and third `u8` parameters (resp.)
/// * `Xu` is the value of the second operand, which must be a `u16` (eXtended unsigned)
/// * `Xs` is the value of the second operand, which must be a `i16` (eXtended signed)
#[packed]
pub enum Opcode {
    /// R[A] := R[B]
    MOV(u8, u8),
    /// R[A] := C[Xu]
    LOADK(u8, u16),

    // Operators

    /// R[A] := R[B] + R[C]
    ADD(u8, u8, u8),
    /// R[A] := R[B] - R[C]
    SUB(u8, u8, u8),
    /// R[A] := R[B] * R[C]
    MUL(u8, u8, u8),
    /// R[A] := R[B] / R[C]
    DIV(u8, u8, u8),
    /// R[A] := R[B] % R[C]
    MOD(u8, u8, u8),
    /// R[A] := R[B] ^ R[C]
    POW(u8, u8, u8),

    /// R[A] := R[B] == R[C]
    EQ(u8, u8, u8),
    /// R[A] := R[B] != R[C]
    NEQ(u8, u8, u8),
    /// R[A] := R[B] <= R[C]
    LEQ(u8, u8, u8),
    /// R[A] := R[B] >= R[C]
    GEQ(u8, u8, u8),
    /// R[A] := R[B] < R[C]
    LESS(u8, u8, u8),
    /// R[A] := R[B] > R[C]
    GREATER(u8, u8, u8),

    // LAND and LOR are implemented with jumps because they can shortcut

    /// R[A] := R[B] & R[C]
    BAND(u8, u8, u8),
    /// R[A] := R[B] | R[C]
    BOR(u8, u8, u8),
    /// R[A] := R[B] ~ R[C]
    BXOR(u8, u8, u8),
    /// R[A] := R[B] << R[C]
    SHIFTL(u8, u8, u8),
    /// R[A] := R[B] >> R[C]
    SHIFTR(u8, u8, u8),

    /// R[A] := -R[B]
    MINUS(u8, u8),
    /// R[A] := !R[B]
    ///
    /// Any "truthy" value will be converted to `false`, the values `false` and `nil` will be
    /// converted to `true`.
    NOT(u8, u8),
    /// R[A] := ~R[B]
    INV(u8, u8),
    /// R[A] := #R[B]
    LEN(u8, u8),

    /// R[A] := R[B] .. R[B+1] .. ... .. R[B+C+1]
    ///
    /// Concatenates the values in register B through B+C+1 (for C==0, concatenates B and B+1)
    CONCAT(u8, u8, u8),
}
