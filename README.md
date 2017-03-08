# `imprint 🐾`

[![Crates.io](https://img.shields.io/crates/v/imprint.svg)](https://crates.io/crates/imprint)
[![Build status](https://travis-ci.org/Rufflewind/imprint.svg?branch=master)](https://travis-ci.org/Rufflewind/imprint)

**Quick links:** [Documentation](https://rufflewind.com/imprint)

A highly experimental Rust library for imprinting values at the type level.

## Usage

Add this to your `Cargo.toml`:

~~~toml
[dependencies]
imprint = "*"
~~~

Next, prepend this to the root module of your crate:

~~~rust
extern crate imprint;
~~~

## Notes

  - [Type-level values](docs/type-level-values.md)
  - [Type equality](docs/type-equality.md)
  - [Indexing](docs/indexing.md)
  - [Existential types](docs/existential-types.md)
  - [Sound logic in a Turing-complete language](docs/sound-logic-in-a-turing-complete-language.md)
  - [Higher-ranked types](docs/higher-ranked-types.md)

## License

Dual-licensed under Apache and MIT.
