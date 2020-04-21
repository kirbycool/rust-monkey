# Rust Monkey

An interpreter for the [monkey programming language](https://monkeylang.org/), written by a beginner to rust.

## What is this?

I wanted to learn rust so I can ride the hype train (choo choo!), and because its safety guarantees and ML inspired syntax appealed to me. Since rust is notorious for its monstorous learning curve, I naturally decided to learn another new thing at the same time: writing an interpreter. I grabbed a copy of the highly recommended "Writing An Interpreter In Go" (https://interpreterbook.com), fired up `rustup docs`, and started typing (er, reading).

This is the finished product! Is it the best rust version of this interpreter? Definitely not. Is it idiomatic and well designed? Probably not, but I tried my best and learned a lot. Does it work? Yes! (I think. Feel free to open a pr ;)

Referencing other implementations helped push me through some tough spots while building this, and hopefully this implementation will help someone else in the future.

## Running

This crate has a single binary, which you can run with

```
cargo run
```

If you want to make a production build, why? But it should work with your normal `cargo build` command. I haven't actually done this yet so I'm not sure what kind of build flags you'd want.

There's also a sizeable test suite you can run with

```
cargo test
```

## Notes for future, bored me

Hey you, bored/inspired person. Here's some more things you can work on:

- Running code from a file
  - Import system
  - Line numbers on errors
- Type system (buy another book)

## Licence

MIT
