#![feature(test)]

extern crate test;
extern crate lea;

use test::Bencher;

use lea::parser::block;

#[bench]
fn parse_simple(b: &mut Bencher) {
    b.iter(|| {
        block(r#"
local i, j, k = 0, 1, 2+3
"#).unwrap();
    });
}
