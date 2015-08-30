//! Contains Lea's `Number` type

// TODO allow adding i64 and f64 to numbers

use std::cmp::Ordering;
use std::hash::*;
use std::fmt;
use std::mem;
use std::ops::*;
use std::num::Wrapping;

use self::Number::*;

pub type LeaInt = i64;
pub type LeaFloat = f64;

#[derive(Copy, Clone, Debug)]
pub enum Number {
    Int(LeaInt),
    Float(LeaFloat),
}

impl fmt::Display for Number {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Int(i) => write!(fmt, "{}", i),
            Float(f) => write!(fmt, "{}", f),
        }
    }
}

/// Manual implementation needed since `Number` can be used as a table key.
impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Number::Int(i) => {
                state.write_u8(0);
                i.hash(state);
            }
            Number::Float(f) => {
                state.write_u8(1);
                state.write_i64(unsafe { mem::transmute(f) });
            }
        }
    }
}

impl<T: Into<Number> + Copy> PartialOrd<T> for Number {
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        match (*self, (*other).into()) {
            (Int(l), Int(r)) => l.partial_cmp(&r),
            (Int(l), Float(r)) => (l as LeaFloat).partial_cmp(&r),
            (Float(l), Int(r)) => l.partial_cmp(&(r as LeaFloat)),
            (Float(l), Float(r)) => l.partial_cmp(&r),
        }
    }
}

impl<T: Into<Number> + Copy> PartialEq<T> for Number {
    fn eq(&self, r: &T) -> bool {
        self.partial_cmp(r) == Some(Ordering::Equal)
    }
}

/// Manual implementation needed since `Number` can be used as a table key. **This isn't really
/// correct, since `NaN` still exists**.
impl Eq for Number {}

impl From<LeaFloat> for Number {
    fn from(f: LeaFloat) -> Self {
        Float(f)
    }
}

impl From<LeaInt> for Number {
    fn from(i: LeaInt) -> Self {
        Int(i)
    }
}

macro_rules! e {
    ($e:expr) => ($e);
}

macro_rules! number_impl {
    ( $tr:ident; $f:ident; ( $($wrap:tt)* ) ) => {
        impl $tr for Number {
            type Output = Self;

            fn $f(self, rhs: Self) -> Self {
                match (self, rhs) {
                    (Int(l), Int(r)) => Int(e!($($wrap)* (l).$f($($wrap)* (r))).0),
                    (Int(l), Float(r)) => Float(e!((l as LeaFloat).$f(r))),
                    (Float(l), Int(r)) => Float(e!(l.$f(r as LeaFloat))),
                    (Float(l), Float(r)) => Float(e!(l.$f(r))),
                }
            }
        }
    };
}

/// Like `number_impl`, but casts its arguments to floats before performing the operation.
macro_rules! number_impl_float {
    ( $tr:ty; $f:ident ) => {
        impl $tr for Number {
            type Output = Self;

            fn $f(self, rhs: Number) -> Number {
                match (self, rhs) {
                    (Int(l), Int(r)) => Float(e!((l as LeaFloat).$f(r as LeaFloat))),
                    (Int(l), Float(r)) => Float(e!((l as LeaFloat).$f(r))),
                    (Float(l), Int(r)) => Float(e!(l.$f(r as LeaFloat))),
                    (Float(l), Float(r)) => Float(e!(l.$f(r))),
                }
            }
        }
    };
}

/// Like `number_impl`, but always casts its arguments to ints
macro_rules! number_impl_int {
    ( $tr:ty; $f:ident; $tf:ident; ( $($wrap:tt)* ) ) => {
        impl $tr for Number {
            type Output = Self;

            fn $f(self, rhs: Self) -> Self {
                match (self, rhs) {
                    (Int(l), Int(r)) => Int(e!($($wrap)* (l).$tf($($wrap)* (r))).0),
                    (Int(l), Float(r)) => Int(e!(l.$tf(r as LeaInt))),
                    (Float(l), Int(r)) => Int(e!((l as LeaInt).$tf(r))),
                    (Float(l), Float(r)) => Int(e!((l as LeaInt).$tf(r as LeaInt))),
                }
            }
        }
    };
}

macro_rules! number_impl_shift {
    ( $tr:ident; $f:ident; $identity:ident; $reverse:ident ) => {
        impl $tr<Number> for Number {
            type Output = Number;

            fn $f(self, rhs: Number) -> Number {
                match (self, rhs) {
                    (Int(l), Int(r)) if r < 0 => Int(l.$reverse((-r) as u32)),
                    (Int(l), Int(r)) => Int(l.$identity(r as u32)),
                    (Int(l), Float(r)) if r < 0.0 => Int(l.$reverse((-r) as u32)),
                    (Int(l), Float(r)) => Int(l.$identity(r as u32)),
                    (Float(l), Int(r)) if r < 0 => Int((l as LeaInt).$reverse((-r) as u32)),
                    (Float(l), Int(r)) => Int((l as LeaInt).$identity(r as u32)),
                    (Float(l), Float(r)) if r < 0.0 => Int((l as LeaInt).$reverse((-r) as u32)),
                    (Float(l), Float(r)) => Int((l as LeaInt).$identity(r as u32)),
                }
            }
        }
    };
}

number_impl!(Add; add; (Wrapping));
number_impl!(Sub; sub; (Wrapping));
number_impl!(Mul; mul; (Wrapping));
number_impl_float!(Div; div);
number_impl_float!(Rem; rem);
number_impl_int!(BitAnd; bitand; bitand; (Wrapping));
number_impl_int!(BitOr; bitor; bitor; (Wrapping));
number_impl_int!(BitXor; bitxor; bitxor; (Wrapping));
number_impl_shift!(Shl; shl; wrapping_shl; wrapping_shr);
number_impl_shift!(Shr; shr; wrapping_shr; wrapping_shl);

impl Neg for Number {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            Int(i) => Int(i.wrapping_neg()),
            Float(f) => Float(-f),
        }
    }
}

impl Not for Number {
    type Output = Self;

    fn not(self) -> Self {
        match self {
            Int(i) => Int(!i),
            Float(f) => Int(!(f as LeaInt)),
        }
    }
}

impl Number {
    /// Exponentiate two numbers
    pub fn pow<T: Into<Self> + Copy>(self, exp: T) -> Self {
        // FIXME Overflow semantics unclear: Rust has no `wrapping_pow`
        match (self, exp.into()) {
            (Int(b), Int(e)) => Int(b.pow(e as u32)),
            (Int(b), Float(e)) => Float((b as LeaFloat).powf(e)),
            (Float(b), Int(e)) => Float(b.powi(e as i32)),
            (Float(b), Float(e)) => Float(b.powf(e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Number::*;

    #[test]
    fn test() {
        let l = Int(5);
        let r = Float(6.5);
        assert_eq!(l, 5);
        assert_eq!(l, 5.0);
        assert_eq!(r, 6.5);
        assert_eq!(l + r, 11.5);
        assert!(l < r);
        assert!(r > l);
        assert!(l < 10);
        assert!(l < 10.1);
        assert!(r < 7);
        assert!(r > 6);
    }
}
