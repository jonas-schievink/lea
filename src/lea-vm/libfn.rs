use value::Value;
use mem::TracedRef;

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

impl From<TracedRef<String>> for LibFnError {
    fn from(s: TracedRef<String>) -> Self {
        LibFnError::Val(Value::TStr(s))
    }
}

impl From<Value> for LibFnError {
    fn from(v: Value) -> Self {
        LibFnError::Val(v)
    }
}

#[derive(Copy)]
pub struct LibFn(pub for<'a, 'b> fn(&'a [Value], &'b mut FnMut(Value)) -> Result<(), LibFnError>);

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
// What follows is an abomination. *It* allows defining library function in a type-safe way. *It*
// also preserves metadata about the functions: Parameters and return values, including their
// declared values, are stored along with the function. *It* is also completely macro-based, so
// inexplicable behaviour is to be expected.
//
// If you want to live a happy life, don't scroll past this. You haven't missed anything. If you
// want to suffer for eternity, please continue and take a look at *it*.
//===============================================================================================

pub enum TyMarker {
    Number,
    String,
    Bool,
    Array,
    Table,
    Any,
    Varargs,
}

pub type LibFnTyInfo = &'static [(
    &'static [(&'static str, TyMarker)],
    &'static [(&'static str, TyMarker)]
)];

pub struct LibFnData {
    pub f: LibFn,
    pub ty_info: LibFnTyInfo,
}

impl LibFnData {
    pub fn get_fn(&self) -> LibFn {
        self.f
    }

    pub fn get_ty_info(&self) -> LibFnTyInfo {
        &self.ty_info
    }
}

pub trait ToValues {
    fn to_values<F>(self, mut push: F) where F: FnMut(Value);
}

impl ToValues for Value {
    #[inline]
    fn to_values<F>(self, mut push: F) where F: FnMut(Value) {
        push(self)
    }
}

impl<'a> ToValues for &'a [Value] {
    #[inline]
    fn to_values<F>(self, mut push: F) where F: FnMut(Value) {
        for value in self {
            push(*value)
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! ident2tym {
    ( number ) => ( $crate::libfn::TyMarker::Number );
    ( string ) => ( $crate::libfn::TyMarker::String );
    ( bool ) => ( $crate::libfn::TyMarker::Bool );
    ( array ) => ( $crate::libfn::TyMarker::Array );
    ( table ) => ( $crate::libfn::TyMarker::Table );
    ( * ) => ( $crate::libfn::TyMarker::Any );
    ( ... ) => ( $crate::libfn::TyMarker::Varargs );
}

#[macro_export]
#[doc(hidden)]
macro_rules! p {
    ( $p:pat ) => ( $p );
}

#[macro_export]
#[doc(hidden)]
macro_rules! build_ty_pat {
    ( [$($p:tt)*] , $($rest:tt)* ) => ( build_ty_pat!([$($p)* ,] $($rest)*) );
    ( [$($p:tt)*] $name:ident : number $($rest:tt)* ) => ( build_ty_pat!([$($p)* $crate::value::Value::TNumber($name)] $($rest)*) );
    ( [$($p:tt)*] $name:ident : string $($rest:tt)* ) => ( build_ty_pat!([$($p)* $crate::value::Value::TStr($name)] $($rest)*) );
    ( [$($p:tt)*] $name:ident : bool $($rest:tt)* ) => ( build_ty_pat!([$($p)* $crate::value::Value::TBool($name)] $($rest)*) );
    ( [$($p:tt)*] $name:ident : array $($rest:tt)* ) => ( build_ty_pat!([$($p)* $crate::value::Value::TArray($name)] $($rest)*) );
    ( [$($p:tt)*] $name:ident : table $($rest:tt)* ) => ( build_ty_pat!([$($p)* $crate::value::Value::TTable($name)] $($rest)*) );
    ( [$($p:tt)*] $name:ident : * $($rest:tt)* ) => ( build_ty_pat!([$($p)* $name] $($rest)*) );
    ( [$($p:tt)*] $name:ident : ... $($rest:tt)* ) => ( build_ty_pat!([$($p)* $name ..] $($rest)*) );
    ( [$($p:tt)*] ) => ( p!([$($p)*]) );
}

#[macro_export]
#[doc(hidden)]
macro_rules! s {
    ( $($s:stmt)* ) => ($($s)*);
}

// FIXME Doesn't handle nested return
#[macro_export]
#[doc(hidden)]
macro_rules! process_body {
    ( [$push_ret:ident] [ $($t:tt)* ] return [ $($e:expr),* ] $($rest:tt)* ) => {
        s!($($t)*)
        {
            $(
                ($e).to_values(|val| $push_ret(val));
            )*
            return Ok(());
        }
        process_body!([$push_ret] [] $($rest)*)
    };
    /*( [$push_ret:ident] [ $($t:tt)* ] { $($c:tt)* } $($rest:tt)* ) => {
        process_body!([$push_ret] [ $($t)* {process_body!([$push_ret] [] $($c)*)} ] $($rest)* );
    };*/
    ( [$push_ret:ident] [ $($t:tt)* ] $next:tt $($rest:tt)*) => {
        process_body!([$push_ret] [$($t)* $next] $($rest)*);
    };
    ( [$push_ret:ident] [$($t:tt)*] ) => {
        s!($($t)*)
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! lea_libfn_single {
    (fn $name:ident {
        $(
            ( $( $pname:ident : $pty:tt ),* ) -> ( $( $rname:ident : $rty:tt ),* ) => { $($body:tt)* }
        )+
    } ) => {
        mod $name {
            use $crate::value::Value;
            use $crate::libfn::LibFnError;

            // Might not be used if the function doesn't return anything
            #[allow(unused_imports)]
            use $crate::libfn::ToValues;

            #[allow(unreachable_code)]  // Don't warn when our inserted return isn't reachable
            pub fn $name(args: &[Value], _push_ret: &mut FnMut(Value))
            -> Result<(), LibFnError> {
                // TODO Figure out a way of matching param types without slice patterns
                match args {
                    $(
                        // The `if true` at the end disables "unreachable pattern" errors
                        build_ty_pat!([] $( $pname : $pty ),*) if true => {
                            process_body!([_push_ret] [] $($body)*);
                            return Ok(());
                        }
                    )+

                    _ => {
                        return Err(format!(
                            "invalid arguments for call to {}",
                            stringify!($name)
                        ).into());
                    }
                }
            }
        }

        #[allow(non_upper_case_globals)]
        static $name: $crate::libfn::LibFnData = $crate::libfn::LibFnData {
            f: $crate::libfn::LibFn($name :: $name),
            ty_info: &[
                $(
                    (
                        &[ $( (stringify!($pname), ident2tym!($pty)) ),* ],
                        &[ $( (stringify!($rname), ident2tym!($rty)) ),* ]
                    )
                ),+
            ],
        };
    };
}

#[macro_export]
macro_rules! lea_libfn {
    ( $( fn $name:ident { $($b:tt)* } )+ ) => {
        $( lea_libfn_single!(fn $name { $($b)* } ); )+
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! setenv {
    ( $env:ident; $gc:ident; $key:ident; $val:expr ) => {
        assert_eq!($env.set(
            Value::TStr($gc.register_obj(stringify!($key).to_string())),
            $val
        ).unwrap(), None);
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! lea_lib_inner {
    ( $env:ident; $gc:ident; $key:ident = fn $f:expr, $($rest:tt)* ) => {
        setenv!($env; $gc; $key; Value::TLibFn($f.get_fn()));
        lea_lib_inner!($env; $gc; $($rest)*);
    };
    ( $env:ident; $gc:ident; $key:ident = str $v:expr, $($rest:tt)* ) => {
        setenv!($env; $gc; $key; Value::TStr($gc.register_obj($v.to_string())));
        lea_lib_inner!($env; $gc; $($rest)*);
    };
    ( $env:ident; $gc:ident; ) => {};
}

#[macro_export]
macro_rules! lea_lib {
    ( $($t:tt)* ) => {
        pub fn init<G: $crate::mem::GcStrategy>(env: &mut $crate::table::Table, gc: &mut G) {
            use $crate::value::Value;

            lea_lib_inner!(env; gc; $($t)*);
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
                    super::vm::value::Value::TNumber(_a) => {
                        if let Some(next) = rest.first() {
                            let rest: &[Value] = &rest[1..];
                            match *next {
                                super::vm::value::Value::TBool(_b) => {
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

#[allow(non_upper_case_globals)]
pub static dummyfn: LibFnData = LibFnData {
    f: vm::libfn::LibFn(dummyfn::dummyfn),
    ty_info: &[
        (&[("a", TyMarker::Number), ("b", TyMarker::Bool), ("c", TyMarker::Varargs)], &[("mynum", TyMarker::Number)]),
        (&[("a", TyMarker::String)], &[("mynum2", TyMarker::Number), ("str", TyMarker::String)]),
    ],
};
*/
