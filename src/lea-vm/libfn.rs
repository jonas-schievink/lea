//! Types and macros for defining library functions in Rust, callable from Lea.

use value::Value;
use mem::{GcStrategy, TracedRef};
use number::*;
use Str;
use VM;

use std::hash::*;
use std::fmt;

/// Kinds of error objects a `LibFn` can return
pub enum LibFnError {
    Val(Value),
    String(String),
}

impl From<String> for LibFnError {
    fn from(s: String) -> Self {
        LibFnError::String(s)
    }
}

impl From<TracedRef<Str>> for LibFnError {
    fn from(s: TracedRef<Str>) -> Self {
        LibFnError::Val(Value::String(s))
    }
}

impl From<Value> for LibFnError {
    fn from(v: Value) -> Self {
        LibFnError::Val(v)
    }
}

/// Newtype of a function reference to be used for library functions.
#[derive(Copy)]
pub struct LibFn(pub for<'a, 'b> fn(&'a mut VM,
                                    arg_start: usize,
                                    arg_count: u8,
                                    &'b mut FnMut(Value))
                                    -> Result<(), LibFnError>);

// Need to implement all of this stuff manually for now (I hope rustc will do that automatically)

impl Clone for LibFn {
    fn clone(&self) -> Self {
        LibFn(self.0)
    }
}

impl Hash for LibFn {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        state.write_usize(self.0 as usize);
    }
}

impl fmt::Pointer for LibFn {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:p}", &self.0)
    }
}

impl fmt::Debug for LibFn {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "LibFn:{:p}", &self.0)
    }
}

impl PartialEq for LibFn {
    fn eq(&self, rhs: &Self) -> bool {
        self.0 as usize == rhs.0 as usize
    }
}

impl Eq for LibFn {}

#[test]
fn size() {
    use std::mem::size_of;
    assert_eq!(size_of::<LibFn>(), size_of::<usize>());
}

//===============================================================================================
// What follows is an abomination. *It* allows defining library functions that pattern-match the
// types of their arguments. *It* also preserves metadata about the functions: Parameters and return
// values, including their declared values, are stored along with the function. *It* is also
// completely macro-based, so prepare for inexplicable behaviour and confusing error messages!
//
// If you want to live a happy life, don't scroll past this. You haven't missed anything. If you
// want to suffer for eternity, please continue and take a look at *it*.
//===============================================================================================

// If *it* has finally consumed you, here are a few todos:
// * Match arguments without slice patterns
//   This is useful since slice patterns are unstable. See the example at the bottom for how this
//   could look without slice patterns (it's not pretty). This could even be made its own crate,
//   since it's not entirely useless. Let's hope LLVM optimizes this right...
// * Autoderef values
//   Currently, if a function matches an argument with `name: string`, the type of name is
//   `TracedRef<Str>`. Autoderef could use the VMs GC to get a `&Str` instead. Smart use of traits
//   makes this easier. If the autoderef'd value is returned, it must be converted back to a Value.

/// Describes how an argument type of a function is matched.
pub enum TyMarker {
    /// `number`
    Number,
    /// `string`
    String,
    /// `bool`
    Bool,
    /// `array`
    Array,
    /// `table`
    Table,
    /// `fn`
    Fn,
    /// `*`: Matches any value
    Any,
    /// `...`: Matches a list of values with any type (or an empty list)
    Varargs,
}

/// Records type information about a function. Currently unused, but might come in handy when
/// autogenerating documentation or performing correctness analyses.
pub type LibFnTyInfo = &'static [(
    &'static [(&'static str, TyMarker)],
    &'static [(&'static str, TyMarker)]
)];

/// Trait for all types that can be used in the `return [...]` of lib functions.
pub trait ToValues {
    fn to_values<F, G: GcStrategy>(self, mut push: F, gc: &mut G) where F: FnMut(Value);
}

impl ToValues for Value {
    fn to_values<F, G: GcStrategy>(self, mut push: F, _: &mut G) where F: FnMut(Value) {
        push(self)
    }
}

/// Passing `Value` slices will return all values inside. Allows to return an argument list matched
/// via `list: ...`.
impl<'a> ToValues for &'a [Value] {
    fn to_values<F, G: GcStrategy>(self, mut push: F, _: &mut G) where F: FnMut(Value) {
        for value in self {
            push(*value)
        }
    }
}

impl ToValues for Number {
    fn to_values<F, G: GcStrategy>(self, mut push: F, _: &mut G) where F: FnMut(Value) {
        push(Value::Number(self))
    }
}

impl ToValues for LeaFloat {
    fn to_values<F, G: GcStrategy>(self, mut push: F, _: &mut G) where F: FnMut(Value) {
        push(Value::Number(self.into()))
    }
}

impl ToValues for LeaInt {
    fn to_values<F, G: GcStrategy>(self, mut push: F, _: &mut G) where F: FnMut(Value) {
        push(Value::Number(self.into()))
    }
}

// These impls are all the same, and an impl for `T: Into<Str>` would be better, but isn't allowed
// due to conflicting impls :(
// (any type could `impl Into<Str>`, including types like `LeaFloat`)
impl ToValues for Str {
    fn to_values<F, G: GcStrategy>(self, mut push: F, gc: &mut G) where F: FnMut(Value) {
        push(Value::String(gc.intern_str(self)))
    }
}
impl<'a> ToValues for &'a str {
    fn to_values<F, G: GcStrategy>(self, mut push: F, gc: &mut G) where F: FnMut(Value) {
        push(Value::String(gc.intern_str(self)))
    }
}
impl ToValues for String {
    fn to_values<F, G: GcStrategy>(self, mut push: F, gc: &mut G) where F: FnMut(Value) {
        push(Value::String(gc.intern_str(self)))
    }
}

/// Convert the Lea type specified in an argument matcher to a `TyMarker`.
#[macro_export]
#[doc(hidden)]
macro_rules! lea_ident_to_ty_marker {
    ( number ) => ( $crate::libfn::TyMarker::Number );
    ( string ) => ( $crate::libfn::TyMarker::String );
    ( bool ) => ( $crate::libfn::TyMarker::Bool );
    ( array ) => ( $crate::libfn::TyMarker::Array );
    ( table ) => ( $crate::libfn::TyMarker::Table );
    ( fn ) => ( $crate::libfn::TyMarker::Fn );
    ( * ) => ( $crate::libfn::TyMarker::Any );
    ( ... ) => ( $crate::libfn::TyMarker::Varargs );
}

/// Interpret the argument as a pattern and return it.
#[macro_export]
#[doc(hidden)]
macro_rules! lea_to_pat {
    ( $p:pat ) => ( $p );
}

/// "Token-Tree muncher" that will convert an argument matcher to a (slice) pattern that can be used
/// inside a Rust `match`.
#[macro_export]
#[doc(hidden)]
macro_rules! lea_build_ty_pat {
    ( [$($p:tt)*] , $($rest:tt)* ) =>
        { lea_build_ty_pat!([$($p)* ,] $($rest)*) };
    ( [$($p:tt)*] $name:ident : number $($rest:tt)* ) =>
        { lea_build_ty_pat!([$($p)* $crate::Value::Number($name)] $($rest)*) };
    ( [$($p:tt)*] $name:ident : string $($rest:tt)* ) =>
        { lea_build_ty_pat!([$($p)* $crate::Value::String($name)] $($rest)*) };
    ( [$($p:tt)*] $name:ident : bool $($rest:tt)* ) =>
        { lea_build_ty_pat!([$($p)* $crate::Value::Bool($name)] $($rest)*) };
    ( [$($p:tt)*] $name:ident : array $($rest:tt)* ) =>
        { lea_build_ty_pat!([$($p)* $crate::Value::Array($name)] $($rest)*) };
    ( [$($p:tt)*] $name:ident : table $($rest:tt)* ) =>
        { lea_build_ty_pat!([$($p)* $crate::Value::Table($name)] $($rest)*) };
    ( [$($p:tt)*] $name:ident : * $($rest:tt)* ) =>
        { lea_build_ty_pat!([$($p)* $name] $($rest)*) };
    ( [$($p:tt)*] $name:ident : ... $($rest:tt)* ) =>
        { lea_build_ty_pat!([$($p)* $name ..] $($rest)*) };
    ( [$($p:tt)*] ) =>
        { lea_to_pat!([$($p)*]) };
}

/// Interprets its argument as a block.
#[macro_export]
#[doc(hidden)]
macro_rules! lea_to_block {
    ( $b:block ) => ($b);
}

// FIXME Doesn't handle nested return
#[macro_export]
#[doc(hidden)]
macro_rules! lea_process_body {
    ( [$push_ret:ident; $vm:ident] [ $($t:tt)* ] return [ $($e:expr),* ] $($rest:tt)* ) => {
        lea_process_body!([$push_ret; $vm] [
            $($t)*
            {
                $(
                    ($e).to_values(|val| $push_ret(val), &mut $vm.gc);
                )*
                return Ok(());
            }
        ] $($rest)*);
    };
    /*( [$push_ret:ident; $vm:ident] [ $($t:tt)* ] { $($c:tt)* } $($rest:tt)* ) => {
        lea_process_body!([$push_ret] [ $($t)* {
            process_body!([$push_ret] [] $($c)*)
        } ] $($rest)* );
    };*/
    ( [$push_ret:ident; $vm:ident] [ $($t:tt)* ] $next:tt $($rest:tt)*) => {
        lea_process_body!([$push_ret; $vm] [$($t)* $next] $($rest)*);
    };
    ( [$push_ret:ident; $vm:ident] [$($t:tt)*] ) => {
        lea_to_block!({ $($t)* })
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! lea_libfn_single {
    (fn $name:ident ($vm:ident) {
        $(
            ( $( $pname:ident : $pty:tt ),* ) ->
            ( $( $rname:ident : $rty:tt ),* ) =>
            { $($body:tt)* }
        )+
    } ) => {
        mod $name {
            use $crate::{VM, Value};
            use $crate::libfn::LibFnError;

            // Might not be used if the function doesn't return anything
            #[allow(unused_imports)]
            use $crate::libfn::ToValues;
            #[allow(unused_imports)]
            use $crate::mem::GcStrategy;

            #[allow(unreachable_code)]  // Don't warn when our inserted return isn't reachable
            // (FIXME: When attributes are allowed on expressions, annotate the `return` instead)
            pub fn $name($vm: &mut VM,
                         arg_start: usize,
                         arg_count: u8,
                         _push_ret: &mut FnMut(Value))
                         -> Result<(), LibFnError> {
                // TODO Figure out a way of matching param types without slice patterns
                match &$vm.stack[arg_start..arg_start+arg_count as usize] {
                    $(
                        // The `if true` at the end disables "unreachable pattern" errors
                        lea_build_ty_pat!([] $( $pname : $pty ),*) if true => {
                            lea_process_body!([_push_ret; $vm] [] $($body)*);
                            return Ok(());
                        }
                    )+

                    _ => {
                        // FIXME This could be a better message
                        return Err(format!(
                            "invalid arguments for call to {}: got {}",
                            stringify!($name),
                            $vm.stack[arg_start..arg_start+arg_count as usize]
                                .iter()
                                .map(|arg| arg.get_type_name())
                                .collect::<Vec<_>>()
                                .join(", ")
                        ).into());
                    }
                }
            }
        }

        #[allow(non_upper_case_globals)]
        pub static $name: $crate::libfn::LibFnTyInfo = &[
            $(
                (
                    &[ $( (stringify!($pname), lea_ident_to_ty_marker!($pty)) ),* ],
                    &[ $( (stringify!($rname), lea_ident_to_ty_marker!($rty)) ),* ]
                )
            ),+
        ];
    };
}

/// Declare a list of library functions callable from Lea code.
///
/// ```rust
/// #![feature(slice_patterns)] // Needed by the macro
/// # #[macro_use] extern crate lea_vm;
///
/// lea_libfn! {
///     fn tostring(vm) {
///         (val: string) -> (s: string) => {
///             return [Value::String(val)]
///         }
///         (val: number) -> (s: string) => {
///             return [format!("{}", val)]
///         }
///     }
/// }
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! lea_libfn {
    ( $( fn $name:ident ($vm:ident) { $($b:tt)* } )+ ) => {
        $( lea_libfn_single!(fn $name ($vm) { $($b)* } ); )+
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! lea_setenv {
    ( $env:ident; $gc:ident; $key:ident; $val:expr ) => {
        assert_eq!($env.set(
            Value::String($gc.intern_str(stringify!($key))),
            $val
        ).unwrap(), None);
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! lea_lib_inner {
    ( $env:ident; $gc:ident; $gcty:ident; $key:ident = fn $f:ident, $($rest:tt)* ) => {
        lea_setenv!($env; $gc; $key; $crate::Value::LibFn($crate::libfn::LibFn($f::$f)));
        lea_lib_inner!($env; $gc; $gcty; $($rest)*);
    };
    ( $env:ident; $gc:ident; $gcty:ident; $key:ident = str $v:expr, $($rest:tt)* ) => {
        lea_setenv!($env; $gc; $key; $crate::Value::String($gc.intern_str($v)));
        lea_lib_inner!($env; $gc; $gcty; $($rest)*);
    };
    ( $env:ident; $gc:ident; $gcty:ident; ) => {};
}

/// Generate a `pub fn init` which will populate a `Table`. This can be used to declare loadable
/// libraries.
///
/// ```rust
/// # #[macro_use] extern crate lea_vm;
/// lea_lib! {
///     some_string = str "I am a string!",
/// }
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! lea_lib {
    ( $($t:tt)* ) => {
        /// Initialize this Lea library by populating the given `Table`. Macro-generated.
        pub fn init<G: $crate::mem::GcStrategy>(env: &mut $crate::Table, gc: &mut G) {
            use $crate::Value;

            lea_lib_inner!(env; gc; G; $($t)*);
        }
    }
}

// Dummy stuff below!

/*
mod dummyfn {
    use super::vm::libfn::LibFnError;

    pub fn dummyfn(args: &[super::vm::value::Value], _push_err: &mut FnMut(super::vm::value::Value))
    -> Result<(), LibFnError> {
        use super::vm::value::Value;

        // This is how slice-pattern-less type matching could look:
        {
            let rest = args;
            if let Some(next) = rest.first() {
                let rest: &[Value] = &rest[1..];
                match *next {
                    super::vm::value::Value::Number(_a) => {
                        if let Some(next) = rest.first() {
                            let rest: &[Value] = &rest[1..];
                            match *next {
                                super::vm::value::Value::Bool(_b) => {
                                    let _c = rest;
                                    {
                                        // First arm body here
                                    }
                                    return Ok(())
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // If we land here, the first arm failed to match

        return Ok(())
    }
}
*/
