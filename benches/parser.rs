#![feature(test)]

extern crate test;
extern crate lea;

use test::Bencher;

use lea::compiler::parser::*;

#[bench]
fn bench_simple(b: &mut Bencher) {
    b.iter(|| {
        block(r#"
local i, j, k = 0, 1, 2+3
i, j = j, i*2
"#).unwrap();
    });
}

#[bench]
fn bench_expression(b: &mut Bencher) {
    b.iter(|| {
        expression("f(1,2,3,4,5,6,6,7,87,8,9,9,5,5,4)").unwrap();
    });
}
