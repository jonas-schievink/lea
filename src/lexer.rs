use std::num::from_str_radix;
use std::str::FromStr;
use std::fmt;

use self::Token::*;
use self::Literal::*;

#[derive(Debug, PartialEq, Clone)]
pub struct LexErr {
    input: String,
    msg: &'static str,
    line: u32,
    col: u32,
}

impl fmt::Display for LexErr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_fmt(format_args!("error at {}:{} on input \"{}\": {}", self.line, self.col,
            self.input, self.msg))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    TInt(i64),
    TFloat(f64),
    TStr(String),
    TBool(bool),
    TNil,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    TLit(Literal),

    // Keywords
    TFunction,
    TDo,
    TIf,
    TThen,
    TElse,
    TElseIf,
    TEnd,
    TWhile,
    TFor,
    TIn,
    TBreak,
    TLocal,
    TReturn,

    // Special
    TIdent(String),
    TNewline,
    TParen0,   // (
    TParen1,   // )
    TSq0,   // [
    TSq1,   // ]
    TCurly0,    // {
    TCurly1,    // }
    TComma, // ,
    TSemi,  // ;
    TColon, // :

    // Operators
    TEq,    // ==
    TNEq,   // !=
    TLEq,   // <=
    TGEq,   // >=
    TLess,  // <
    TGreater,   // >

    TLAnd,  // &&
    TLOr,   // ||
    TLNot,  // !
    TBAnd,  // &
    TBOr,   // |
    TBXor,  // ^
    TBNot,  // ~

    TAdd,   // +
    TSub,   // -
    TMul,   // *
    TDiv,   // /
    TMod,   // %
    TLen,   // #    length operator

    TDot,   // .    dot-index operator

    /// Invalid token with error message
    TInv(Box<LexErr>),
}

static KEYWORDS: ::phf::Map<&'static str, Token> = phf_map! {
    "function" => TFunction,
    "do" => TDo,
    "if" => TIf,
    "then" => TThen,
    "else" => TElse,
    "elseif" => TElseIf,
    "end" => TEnd,
    "while" => TWhile,
    "for" => TFor,
    "in" => TIn,
    "break" => TBreak,
    "local" => TLocal,
    "return" => TReturn,
    "nil" => TLit(TNil),
    "true" => TLit(TBool(true)),
    "false" => TLit(TBool(false)),
};

rustlex! Lexer {
    property line: u32 = 1;
    property relpos: u32 = 0;

    let WHITESPACE = [' ''\t']+;

    let DIGIT_DEC = ['0'-'9'];
    let DIGIT_HEX = ['0'-'9''a'-'f''A'-'F'];
    let DIGIT_OCT = ['0'-'7'];

    let INT_DEC = '-'? DIGIT_DEC+;
    let INT_HEX = '-'? "0x" DIGIT_HEX+;
    let INT_OCT = '-'? "0o" DIGIT_OCT+;

    let CFLOAT = DIGIT_DEC+ '.' | DIGIT_DEC* '.' DIGIT_DEC+;
    let EFLOAT = ( CFLOAT | DIGIT_DEC+ ) 'e' ["+-"]? DIGIT_DEC+;
    let FLOAT = '-'? ( CFLOAT | EFLOAT );

    let COMMENT = "//" [^'\n']*;
    let COMMENT_BLOCK = "/*" ([^'*']|'*'[^'/'])* "*/";

    let IDENT = ['_''a'-'z''A'-'Z']['_''a'-'z''A'-'Z''0'-'9']+;
    let STRING = '"' ([^'\\''"']|'\\'.)* '"';

    let ANY = .;

    // Unknown character
    ANY => |&: lexer: &mut Lexer<R>| {
        let yystr = lexer.yystr();
        lexer.generr(yystr.clone(), "unknown character")
    }

    // Ignore these matches
    WHITESPACE => |&: _lexer: &mut Lexer<R>| None
    COMMENT => |&: _lexer: &mut Lexer<R>| None
    COMMENT_BLOCK => |&: _lexer: &mut Lexer<R>| None

    '\n' => |&: lexer: &mut Lexer<R>| {
        lexer.line += 1;
        lexer.relpos = lexer._input.pos.off as u32;

        Some(TNewline)
    }

    '(' => |&: _lexer: &mut Lexer<R>| Some(TParen0)
    ')' => |&: _lexer: &mut Lexer<R>| Some(TParen1)
    '[' => |&: _lexer: &mut Lexer<R>| Some(TSq0)
    ']' => |&: _lexer: &mut Lexer<R>| Some(TSq1)
    '{' => |&: _lexer: &mut Lexer<R>| Some(TCurly0)
    '}' => |&: _lexer: &mut Lexer<R>| Some(TCurly1)
    ',' => |&: _lexer: &mut Lexer<R>| Some(TComma)
    ';' => |&: _lexer: &mut Lexer<R>| Some(TSemi)
    ':' => |&: _lexer: &mut Lexer<R>| Some(TColon)

    "==" => |&: _lexer: &mut Lexer<R>| Some(TEq)
    "!=" => |&: _lexer: &mut Lexer<R>| Some(TNEq)
    "<=" => |&: _lexer: &mut Lexer<R>| Some(TLEq)
    ">=" => |&: _lexer: &mut Lexer<R>| Some(TGEq)
    "<" => |&: _lexer: &mut Lexer<R>| Some(TLess)
    ">" => |&: _lexer: &mut Lexer<R>| Some(TGreater)

    "&&" => |&: _lexer: &mut Lexer<R>| Some(TLAnd)
    "||" => |&: _lexer: &mut Lexer<R>| Some(TLOr)
    "!" => |&: _lexer: &mut Lexer<R>| Some(TLNot)
    "&" => |&: _lexer: &mut Lexer<R>| Some(TBAnd)
    "|" => |&: _lexer: &mut Lexer<R>| Some(TBOr)
    "^" => |&: _lexer: &mut Lexer<R>| Some(TBXor)
    "~" => |&: _lexer: &mut Lexer<R>| Some(TBNot)

    "+" => |&: _lexer: &mut Lexer<R>| Some(TAdd)
    "-" => |&: _lexer: &mut Lexer<R>| Some(TSub)
    "*" => |&: _lexer: &mut Lexer<R>| Some(TMul)
    "/" => |&: _lexer: &mut Lexer<R>| Some(TDiv)
    "%" => |&: _lexer: &mut Lexer<R>| Some(TMod)
    "#" => |&: _lexer: &mut Lexer<R>| Some(TLen)

    "." => |&: _lexer: &mut Lexer<R>| Some(TDot)

    INT_DEC => |&: lexer: &mut Lexer<R>| {
        use std::old_io::stdio;

        let yystr = lexer.yystr();
        let mut neg = 1;
        let mut start = 0;

        stdio::println(yystr.as_slice());

        match yystr.as_slice().char_at(0) {
            '-' => {
                neg = -1;
                start = 1;
            },
            '+' => {
                start = 1;
            }
            _ => {},
        }

        match yystr.as_slice()[start..].parse() {
            Some(i) => Some(TLit(TInt(neg * i))),
            None => lexer.generr(yystr.clone(), "invalid int literal"),
        }
    }

    INT_HEX => |&: lexer: &mut Lexer<R>| {
        let yystr = lexer.yystr();
        let mut neg = 1;
        let mut start = 0;

        match yystr.as_slice().char_at(0) {
            '-' => {
                neg = -1;
                start = 1;
            },
            '+' => {
                start = 1;
            }
            _ => {},
        }

        match from_str_radix(&yystr.as_slice()[start+2..], 16) {
            Some(i) => Some(TLit(TInt(neg * i))),
            None => lexer.generr(yystr.clone(), "invalid hex literal"),
        }
    }

    INT_OCT => |&: lexer: &mut Lexer<R>| {
        let yystr = lexer.yystr();
        let mut neg = 1;
        let mut start = 0;

        match yystr.as_slice().char_at(0) {
            '-' => {
                neg = -1;
                start = 1;
            },
            '+' => {
                start = 1;
            }
            _ => {},
        }

        match from_str_radix(&yystr.as_slice()[start+2..], 8) {
            Some(i) => Some(TLit(TInt(neg * i))),
            None => lexer.generr(yystr.clone(), "invalid oct literal"),
        }
    }

    FLOAT => |&: lexer: &mut Lexer<R>| {
        let yystr = lexer.yystr();

        match FromStr::from_str(yystr.as_slice()) {
            Some(i) => Some(TLit(TFloat(i))),
            None => lexer.generr(yystr.clone(), "invalid float literal"),
        }
    }

    IDENT => |&: lexer: &mut Lexer<R>| {
        let yystr = lexer.yystr();
        match KEYWORDS.get(yystr.as_slice()) {
            Some(tok) => Some(tok.clone()),
            None => Some(TIdent(yystr)),
        }
    }

    STRING => |&: lexer: &mut Lexer<R>| {
        // Strip '"' off
        let yystr = lexer.yystr();
        let sliced = &yystr.as_slice()[1..yystr.len()-1];

        // Replace escape sequences
        let mut newstr = String::with_capacity(sliced.len());
        let mut esc = false;
        for ch in sliced.chars() {
            if esc {
                // Find replacement char ('?' means invalid esc. seq.)
                let replacement = match ch {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    _ => '?',
                };

                if replacement == '?' {
                    return lexer.generr(yystr.clone(), "invalid escape sequence");
                } else {
                    newstr.push(replacement);
                }

                esc = false;
            } else {
                if ch == '\\' { esc = true; }
                else { newstr.push(ch); }
            }
        }

        Some(TLit(TStr(newstr)))
    }
}

impl <R> Lexer<R> {
    fn generr(&self, input: String, msg: &'static str) -> Option<Token> {
        Some(TInv(Box::new(LexErr {
            input: input,
            msg: msg,
            line: self.line,
            col: self._input.pos.off as u32 - self.relpos,
        })))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Token::*;
    use super::Literal::*;

    use std::str::FromStr;
    use std::old_io::BufReader;

    fn lex(input: &str) -> Lexer<BufReader> {
        let rd = BufReader::new(input.as_bytes());
        Lexer::new(rd)
    }

    fn test(input: &str, expect: &Vec<Token>) {
        let mut it = expect.iter();
        for tok in lex(input) {
            let exp = it.next();

            match exp {
                Some(exptok) => {
                    if *exptok != tok {
                        panic!("Unexpected token: Got {:?}, expected {:?}", tok, exptok);
                    }
                },
                None => panic!("Got unexpected additional token: {:?}", tok),
            }
        }

        let next = it.next();
        match next {
            Some(token) => {
                panic!("Unexpected end of token stream; expected {:?}", token);
            },
            _ => {},
        }
    }

    #[test]
    fn ints() {
        test("0", &vec![TLit(TInt(0))]);
        test("0 0", &vec![TLit(TInt(0)), TLit(TInt(0))]);
        test("0001 1 987123 -1234 1234", &vec![TLit(TInt(1)), TLit(TInt(1)), TLit(TInt(987123)),
            TLit(TInt(-1234)), TLit(TInt(1234))]);
        test("0x0 0xff 0xffff 0xabcdef -0xabcdef", &vec![TLit(TInt(0x0)), TLit(TInt(0xff)),
            TLit(TInt(0xffff)), TLit(TInt(0xabcdef)), TLit(TInt(-0xabcdef))]);
        test("0o123 0o0 -0o765", &vec![TLit(TInt(0o123)), TLit(TInt(0)), TLit(TInt(-0o765))]);
    }

    #[test]
    fn floats() {
        // If this fails due to rounding errors, sacrifice a goat and try again
        test("0.0 0.5 0.8 1.0 1.67 54321.5 -0.5 -12345.0", &vec![
            TLit(TFloat(0.0)), TLit(TFloat(0.5)), TLit(TFloat(0.8)), TLit(TFloat(1.0)),
            TLit(TFloat(FromStr::from_str("1.67").unwrap())), TLit(TFloat(54321.5)),
            TLit(TFloat(-0.5)), TLit(TFloat(-12345f64)),
        ]);
    }

    #[test]
    fn floats_exp() {
        test("4e2 -2e-2 543e012 543e+12 -0.5e-4 -3e1 1e1", &vec![
            TLit(TFloat(4e+2)), TLit(TFloat(-2e-2)), TLit(TFloat(543e+12)), TLit(TFloat(543e+12)),
            TLit(TFloat(-0.5e-4)), TLit(TFloat(-30.0)), TLit(TFloat(10.0)),
        ]);
    }

    #[test]
    fn keywords() {
        test("0function", &vec![TLit(TInt(0)), TFunction]);
        test("function0", &vec![TIdent(String::from_str("function0"))]);
        test("function do while if then else elseif end for in break local return nil true false",
        &vec![
            TFunction, TDo, TWhile, TIf, TThen, TElse, TElseIf, TEnd, TFor, TIn, TBreak, TLocal,
            TReturn, TLit(TNil), TLit(TBool(true)), TLit(TBool(false)),
        ]);
    }

    #[test]
    fn strings() {
        test(r#" "test" "test\n\\\r\\\r\t\n" "#, &vec![
            TLit(TStr(String::from_str("test"))),
            TLit(TStr(String::from_str("test\n\\\r\\\r\t\n"))),
        ]);
    }

    #[test]
    fn comments() {
        test("5//srvever__.7\n/**//**** /*/8", &vec![
            TLit(TInt(5)), TNewline, TLit(TInt(8)),
        ]);
    }

    #[test]
    fn special() {
        test("== != >= <= < > ! ~ + - * / % ^ # . : ; && & || | ()[]{}", &vec![
            TEq, TNEq, TGEq, TLEq, TLess, TGreater, TLNot, TBNot, TAdd, TSub, TMul, TDiv, TMod,
            TBXor, TLen, TDot, TColon, TSemi, TLAnd, TBAnd, TLOr, TBOr, TParen0, TParen1, TSq0,
            TSq1, TCurly0, TCurly1,
        ]);
    }
}
