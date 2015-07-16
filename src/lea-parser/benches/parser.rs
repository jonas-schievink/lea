#![feature(test)]

extern crate test;
extern crate lea_parser;

use test::Bencher;


#[bench]
fn simple(b: &mut Bencher) {
    let code = r#"
local i, j, k = 0, 1, 2+3
i, j = j, i*2
"#;

    b.bytes = code.len() as u64;
    b.iter(|| {
        lea_parser::block(code).unwrap();
    });
}

#[bench]
fn expression(b: &mut Bencher) {
    let code = "f(1,2,3,4,5,6,6,7,87,8,9,9,5,5,4)";

    b.bytes = code.len() as u64;
    b.iter(|| {
        lea_parser::expression(code).unwrap();
    });
}
