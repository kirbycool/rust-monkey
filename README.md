# Rust Monkey

An interpreter for the [monkey programming language](https://monkeylang.org/).

## What is this?

This is a Rust implementation of the Monkey toy programming language from "Writing An Interpreter In Go" (https://interpreterbook.com). It was a great way for me to learn both Rust and interpreters at the same time 

## Running

This crate has a single binary, which you can run with

```
cargo run
```

I wouldn't recommend using this in any production setting, but you can build it with your normal `cargo build` command.

There's also a test suite you can run with

```
cargo test
```

## Notes for future me

Here's some ideas for future improvements

- Running code from a file
  - Import system
  - Line numbers on errors
- Type system (buy another book)

## Licence

MIT
