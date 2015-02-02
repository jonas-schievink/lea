use parser::{ident, integer, float, string, literal};

#[test]
fn integers() {
    assert_eq!(integer("0"), Ok(0));
    assert_eq!(integer("1"), Ok(1));
    assert_eq!(integer("-1"), Ok(-1));
    assert_eq!(integer("9999999999"), Ok(9999999999i64));
    assert_eq!(integer("-9999999999"), Ok(-9999999999i64));

    assert_eq!(integer("0x00"), Ok(0));
    assert_eq!(integer("-0x00"), Ok(0));
    assert_eq!(integer("0xff"), Ok(255));
    assert_eq!(integer("-0xff"), Ok(-255));
    assert_eq!(integer("0xffffffffff"), Ok(0xffffffffff));

    assert_eq!(integer("0o76"), Ok(0o76));
    assert_eq!(integer("-0o76"), Ok(-0o76));
}

#[test]
fn floats() {
    assert_eq!(float("0.0"), Ok(0.0));
    assert_eq!(float("-0.0"), Ok(-0.0));
    assert_eq!(float("10000.0"), Ok(10000.0));

    assert_eq!(float("1e4"), Ok(1e+4));
    assert_eq!(float("1e+4"), Ok(1e+4));
    assert_eq!(float("-1e+4"), Ok(-1e+4));
    assert_eq!(float("2e-4"), Ok(2e-4));
}

#[test]
fn idents() {
    assert_eq!(ident("test"), Ok("test"));
    assert_eq!(ident("_a0_0_"), Ok("_a0_0_"));
    assert_eq!(ident("_"), Ok("_"));
    assert_eq!(ident("a"), Ok("a"));
    assert_eq!(ident("_0"), Ok("_0"));

    assert!(ident("0a").is_err());
    assert!(ident("ä").is_err());
    assert!(ident("7").is_err());
}

#[test]
fn strings() {
    assert_eq!(string("\"\""), Ok("".to_string()));
    assert_eq!(string("\" test_string \""), Ok(" test_string ".to_string()));
    assert_eq!(string("\"\\n\\r\\t\\\\\""), Ok("\n\r\t\\".to_string()));

    // TODO: unescaped "\n" is accepted; keep? change? remove? multiline str?
    assert_eq!(string("\"\n\""), Ok("\n".to_string()));

    // invalid escape seq
    assert!(string("\"\\q\"").is_err());
}

#[test]
fn literals() {
    use lexer::Literal::*;

    assert_eq!(literal("nil"), Ok(TNil));
    assert_eq!(literal("false"), Ok(TBool(false)));
    assert_eq!(literal("true"), Ok(TBool(true)));
    assert_eq!(literal("\"false\\n\""), Ok(TStr("false\n".to_string())));
    assert_eq!(literal("0"), Ok(TInt(0)));
    assert_eq!(literal("0.0"), Ok(TFloat(0.0)));
    assert_eq!(literal("7.5e+5"), Ok(TFloat(7.5e+5)));
}
