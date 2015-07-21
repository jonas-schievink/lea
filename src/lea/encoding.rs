//! Defines various encoders for outputting the compiled `FnData`

use compiler::FnData;

use bincode::{self, SizeLimit, EncodingError};

use rustc_serialize::json;

use std::io::{self, Write};

macro_rules! encoders {
    ( $($name:ident ( $fndata:ident, $wr:ident ) => $b:block,)* ) => (
        #[derive(Debug, RustcDecodable)]
        #[allow(non_camel_case_types)]
        pub enum Encoding {
            $($name),*
        }

        impl Encoding {
            pub fn encode<W: Write>(&self, fndata: &FnData, wr: &mut W)
            -> io::Result<()> {
                match *self {
                    $(
                    Encoding::$name => {
                        let $fndata = fndata;
                        let $wr = wr;
                        $b
                    }
                    )*
                }
            }
        }
    )
}

encoders! {
    json(fndata, wr) => {
        match json::encode(fndata) {
            Ok(s) => Ok(try!(write!(wr, "{}", &s))),
            Err(_) => Err(io::Error::new(io::ErrorKind::Other, "json encoding error")),
        }
    },
    bin(fndata, wr) => {
        match bincode::encode_into(fndata, wr, SizeLimit::Infinite) {
            Ok(_) => Ok(()),
            Err(EncodingError::IoError(e)) => Err(e),
            Err(EncodingError::SizeLimit) => Err(io::Error::new(io::ErrorKind::Other, "size limit reached")),
        }
    },
    debug(fndata, wr) => {
        write!(wr, "{:#?}\"", fndata)
    },
}
