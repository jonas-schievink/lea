# Lea - A sane Lua-like language
[![Build Status](https://travis-ci.org/jonas-schievink/lea.svg?branch=master)](https://travis-ci.org/jonas-schievink/lea)

[Documentation](https://jonas-schievink.github.io/lea/lea/index.html)

**Note: This is a work-in-progress. Nothing runs yet. Many features described here are not yet implemented**

Lea is a programming language derived from Lua 5.3 and written in Rust. Lea offers several slight changes to the syntax, making it more familiar, as well as a reworked standard library that offers much more functionality.

For `.lua` files, Lea might later gain a **compatibility mode** that aims to act as a drop-in replacement for the reference Lua interpreter (the syntax is already almost identical).

When used in the "default" mode (with the Lea modifications enabled), the compiler will apply a different set of Lints to discourage the use of deprecated Lua syntax or discouraged coding patterns (such as assignment to global variables).

The replacement standard library is a reimplementation of the Lua stdlib, meaning that all Lua code will still work with it (assuming the rest of the compatiblity mode is implemented). It is planned that it will also offer an FFI (though maybe not compatible to LuaJITs FFI), which allows integrating external libraries without writing a single line of C or Rust code (everything can be done in Lea).

Lea makes the following changes to plain Lua 5.3:
* `!=` to replace `~=` (the latter is still supported, but will cause a warning if not running in compatibility mode)
* Replace "word" operators with commonly used symbols (`and` -> `&&`, `or` -> `||`, `not` -> `!`; as with `!=`, the keywords are still supported)
* Tables split into arrays and associative arrays (classical tables)
* 0 based arrays
* Larger standard library (including an FFI, extended filesystem functions, networking, ...; these features will be largely wrappers aroung Rust's standard library functions)

# Note

This project does not have the same goal as Lua does. Lua is a very minimal language (including its reference implementation). Lea is basically the same language, but the implementation is much bigger, since the library provides an AST, multiple transformation passes, and a seperate bytecode emission pass. Lua does all of this basically while it's parsing the source code, which makes it much faster, but prevents operations on the whole AST (since there is none).

I chose this approach to make the project more modular and easier to understand. This also makes the Lea (byte code) compiler more flexible (for example, different stdlibs can register different Lints to emit warnings/errors when deprecated functions are used).
