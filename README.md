# Lea - A sane Lua-like language [![Build Status](https://travis-ci.org/jonas-schievink/lea.svg?branch=master)](https://travis-ci.org/jonas-schievink/lea)

[Documentation](https://jonas-schievink.github.io/lea/lea/index.html)

**Note: This is a work-in-progress. Nothing runs yet. Many features described here are not yet implemented**

Lea is a programming language derived from Lua 5.3 and written in Rust. Lea offers several slight changes to the syntax, making it more familiar, as well as a reworked standard library that makes programs safer and offers much more functionality.

For `.lua` files, Lea offers a **compatibility mode** that aims to act as a drop-in replacement for the reference Lua interpreter.

When used in the "default" mode (with the Lea modifications enabled), the compiler will apply a different set of Lints to discourage the use of deprecated Lua syntax.

The replacement standard library is a superset of the Lua library, meaning that all Lua code should still work with it. It will contain an FFI, which allows integrating external libraries without writing a single line of C or Rust code (everything can be done in Lea).

The following features / changes are made by Lea (note that these are - of course - not objectively better, but merely resemble my own opinion):

* `!=` to replace `~=`
* Integer type (Lua 5.3 has this too)
* Replace "word" operators with commonly used symbols (`and` -> `&&`, `or` -> `||`, `not` -> `!`)
* Integrated bitwise operators (compatible to Lua 5.3)
* C-style comments instead of Lua's `--` syntax
* Tables split into arrays and associative arrays (classical tables)
* 0 based arrays
* Larger standard library (including an FFI, extended filesystem functions, etc.)

# Note

This project does not have the same goal as Lua does. Lua is a very minimal language (including its reference implementation). Lea is basically the same language, but the implementation is much bigger, since the library provides an AST, multiple transformation passes, and a seperate bytecode emission pass. Lua does all of this basically while it's parsing the source code, which makes it much faster, but prevents operations on the whole AST (since there is none).

## Inequality operator `!=`

Lua is one of the few languages that use `~=` for inequality testing. Replacing the token with the much more commonly used `!=` makes Lea easier to learn for people coming from a C-like language that also uses `!=`.

## Integer type

Lua didn't have an integer type until version `5.3`. All numbers were handled as 64-bit floating point values. For applications requiring more than the available 52 mantissa bits, this was an issue, as they had to use big integers or another mechanism. Embedded systems without a floating-point unit also suffered from performance problems due to this.

These issues were addressed with Lua 5.3, where not only an integer type was introduced, but also support for building Lua without float values was added, making Lua more friendly to embedded systems.

## Replace word operators

Lua provides the operator keywords `and`, `or` and `not` to denote logical operators. Lea replaces them with the tokens `&&`, `||` and `!` to make it more similar to C-like languages.

## Bitwise operators

Lua didn't provide bitwise operators until the `bit32` module was added to Lua 5.2, and has gained "real" bitwise operators with Lua 5.3.

Lea also integrates bitwise operators into the language. They will use the tokens `&`, `|`, `~` to mean "and", "or", and "complement" or "xor" (depending on whether `~` is used as a unary or binary operator). The bitwise operators will only work on integers and are compatible to the bitwise operators introduced with Lua 5.3.

## Comments

Like many changes, this change is made to make Lua more similar to C-like languages. Instead of introducing comments with `--` or `--[[` (for multiline comments), Lea will use C's comment style: `//` and `/*`, `*/`.

Comments are not yet included in the AST, but when they are, a Lint will warn if Lua-style comments are used in Lea code.

## Distinct array type

Lua provides one central data structure: Tables. Tables have an "array part" which stores values linearly in memory. They also have a "hash part" that works like a hash table: You can store key-value pairs and look up the value if you have its key.

This makes Lua's design very simple and yet flexible: Tables can be used for namespaces (thanks to the `.` access syntax sugar) and to store data the program works with. They can be used as building blocks for more complicated data structures, if needed.

Lea will provide a table type that can be used like Lua's tables: It supports metatables, weak reference modes, and will automatically augment its array part.

In Lua, the lenth operator will return the number of entries in a table's array part. This allows iterating over the array part with a numeric for loop, ignoring the hash part, but is a bit unintuitive, since `#{key = "test"}` evaluates to 0. Lea will return the number of mappings in the table instead. To avoid breaking Lua code, a compatibility mode can be activated when compiling a `.lua` file, which will emulate Lua behaviour.

It also provides a distinct array type that stores an ordered list of values indexed by integers starting at 0 (see below). The array type is simpler and more lightweight than the table type (no support for metatables and other table features).

The distinct array type will be subject to a few optimizations. Dynamically typed languages usually implement arrays in a way that allows the user to store mixed types. The array has to store the type of every single element, which is quite costly is terms of memory. As long as all elements are of the same type, the type would only need to be stored once. Additionally, this allows the use of special data layouts depending on the element type:

An array of booleans could be implemented as a bit map, resulting in a 98% decrease in memory usage (assuming the boolean would otherwise take 64 bits, which is the case in a naive implementation).

An array of integers in the range 0..255 could be implemented as a byte array, reducing the overhead to a constant amount.

## 0-based indexing

Lua uses 1 as the index for the first element in an array. While this is logical from an "outsider" perspective, it doesn't make sense when looking at it as a programmer: The first element in an array is stored at offset 0.

Lea strictly sees array indices as offsets and starts indexing at 0.

## Larger standard library

Lua was designed to be a very small language with a minimal runtime. This comes at the expense of a quite small runtime library, which is sometimes lacking features.

Lea will provide a larger runtime library (mainly wrappers around the Rust stdlib):
* **FFI** - A Foreign Function Interface for loading and calling C code at runtime makes Lua-typical wrapper modules written in C obsolete. Lua provides an FFI via third-party modules.
* **Filesystem** - Lua's filesystem API is limited to simple operations, as more complex scenarios are not portable and rely on OS-specific features. Lea will provide a more complete API, including support for iterating over directories, changing the current directory, locking files, checking file permissions, etc. built on top of rust's stdlib
