# Lea - A sane Lua-based language written in Rust

* `!=` to replace `~=`
* Integer type (Lua 5.3 has this too)
* Replace "word" operators with commonly used symbols (`and` -> `&&`, `or` -> `||`, ...)
* Integrated bitwise operators (`&`, `|`, `~`, `^`)
* Replace the power operator `^` with XOR
* C-style comments instead of Lua's `--` syntax
* Tables split into arrays and associative arrays (classical tables)
* 0 based arrays
* Larger standard library (including an FFI, filesystem functions, etc.)


## Inequality operator `!=`

Lua is one of the few languages that use `~=` for inequality testing. Replacing the token with the much more commonly used `!=` makes Lea easier to learn for people coming from a C-like language that also uses `!=`.

## Integer type

Lua didn't have an integer type until version `5.3`. All numbers were handled as 64-bit floating point values. For applications requiring more than the available 52 mantissa bits, this was an issue, as they had to use big integers or another mechanism. Embedded systems without a floating-point unit also suffered from performance problems due to this.

These issues were addressed with Lua 5.3, where not only an integer type was introduced, but also support for building Lua without float values was added, making Lua more friendly to embedded systems.

## Replace word operators

Lua provides the operator keywords `and`, `or` and `not` to denote logical operators. Lea replaces them with the tokens `&&`, `||` and `!` to make it more similar to C-like languages.

## Bitwise operators

Lua didn't provide bitwise operators until the `bit32` module was added to Lua 5.2. Even then, the module only allows accessing 32 bits of individual numbers (as all numbers are stored as floats).

Lea integrates bitwise operators into the language. They will use the tokens `&`, `|`, `~`, `^` to mean "and", "or", "complement" and "xor". The bitwise operators will only work on integers. This makes Lea more similar to C (and C-inspired languages).

## Power operator

Lua provides the "power operator" `^`, which raises its left operand to the power of the right operand. This makes sense for a language like Lua, which is used in a mathematical context. However, the `xor` operation might be used more commonly when the language is used in a programming context.

Lea replaces the function of the `^` token to mean "bitwise exclusive-or". This makes the meaning of `^` more coherent with the added bitwise operators (see above).

## Comments

Like many changes, this change is made to make Lua more similar to C-like languages. Instead of introducing comments with `--` or `--[[` (for multiline comments), Lea will use C's comment style: `//` and `/*`, `*/`.

## Distinct array type

Lua provides one central data structure: Tables. Tables have an "array part" which stores values linearly in memory. They also have a "hash part" that works like a hash table: You can store key-value pairs and look up the value if you have its key.

This makes Lua's design very simple and yet flexible: Tables can be used for namespaces (thanks you the `.` access syntax sugar) and to store data the program works with. They can be used as building blocks for more complicated data structures, if needed.

Lea will provide a table type that is used like the hash part of Lua's tables (an associative array). It also provides a distinct array type that stores an ordered list of values indexed by integers starting at 0 (see below).

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
* **Filesystem** - Lua's filesystem API is limited to simple operations, as more complex scenarios are not portable and rely on OS-specific features. Lea will provide a more complete API, including support for iterating over directories, changing the current directory, locking files, checking file permissions, etc.
