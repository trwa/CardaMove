# CardaMove

Simple POC for a transpiler from [Move](https://move-language.github.io/move/) to [Aiken](https://aiken-lang.org/).

## Milestone 0

Parse the following Move type definition:

```rust
struct T has key, drop, copy {
    f0: bool,
    f1: bool,
}
```

Into some kind of AST.
