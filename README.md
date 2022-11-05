## Table of contents

- [Table of contents](#table-of-contents)
- [fp programming language](#fp-programming-language)
- [Examples of `fp`](#examples-of-fp)
- [Usage](#usage)
  - [Command line](#command-line)
  - [Interpret](#interpret)
  - [REPL](#repl)
- [Documentation](#documentation)
- [Development](#development)
  - [Nix support](#nix-support)
  - [Tips](#tips)
- [Credits](#credits)

## fp programming language

`fp` is a programming language heavily inspired by the language John Backus
described in his 1977 Turing Award lecture.

The paper can be found [here](https://dl.acm.org/doi/10.1145/359576.359579).

## Examples of `fp`

```haskell
{- Matrix multiplication.
-}

Def ip ≡ /+∘α*∘⍉

Def mm ≡ α(α ip) ∘ α distl ∘ distr ∘ [~0, ⍉∘~1]

mm:< < <1,2>, <4,5> >,
     < <6,8>, <7,9>> >
```

## Usage

This section will give a quick tour of many of the language features of `fp`. It
will also cover the usage of the tools provided by `fp`.

### Command line

`fp` can be used without explicitly installing it via nix!

```
nix run github:japiirainen/fp -- --help
                 
Up to date
Usage: fp COMMAND

  Command-line utility for the `fp` programming language

Available options:
  -h,--help                Show this help text

Available commands:
  interpret                Interpret a `fp` file
  repl                     Enter a REPL for `fp`
```

### Interpret

The `interpret` command can be used to interpret `fp` files.

```haskell
Def ip ≡ /+∘α*∘⍉

ip:<<1,2,3>,<6,5,4>>
```

This program lives in `examples/ip.fp` and can be interpreted like this.

```haskell
cabal run fp -- interpret examples/ip.fp
```

Which will yield `28`.

### REPL

you can enter the `fp` repl to get an interactive environment:

```sh
fp repl
```

```haskell
λ +:<1,2>
3
λ :let xs = <1,2,3>
λ xs
<1,2,3>
```

## Documentation

Currently the `examples` directory serves as the documentation! I will list some
important topics below for reference.

- [Conditionals](./examples/condition.fp)
`Fp` has a condition expression. It is similar to ternary operator in many
ordinary languages.

- [While](./examples/while.fp)
`while` provides a way to run a specific program many times, specifically
until some condition is met.

- [Binary to unary](./examples/bu.fp)
`bu` gives a convenient way to turn binary (2 argument) functions
into unary (1 argument) functions. This is kind of like partial
application.

- [Matrix multiplication](./examples/mm.fp)
This example shows how to do matrix multiplication in `fp`.

- [Factorials](./examples/fact.fp)
A way to compute factorials in `fp`.

Here's a bunch of primitive functions.

- [boolean algebra](./examples/and-or-not.fp)
- [append](./examples/append.fp)
- [applyToAll](./examples/applyToAll.fp)
- [atom](./examples/atom.fp)
- [const](./examples/const.fp)
- [construction](./examples/construction.fp)
- [dist](./examples/dist.fp)
- [eq](./examples/eq.fp)
- [id](./examples/id.fp)
- [length](./examples/length.fp)
- [nth](./examples/nth.fp)
- [null](./examples/null.fp)
- [reverse](./examples/reverse.fp)
- [transpose](./examples/transpose.fp)
- [rotate](./examples/rotate.fp)

- [Unbound variable error](./examples/fact.fp)
`Fp` also has nice error messages.

## Development

You can also run the test suite.

```sh
cabal test tasty
```

### Nix support

You can alternatively use nix for dev environment and for building the project.

Build:

```sh
nix build .
```

Run:

```sh
nix run .
```

Start Nix shell:

```sh
nix-shell
```

### Tips

- Run `nix flake update` to update all flake inputs.
- Run `./bin/hoogle` to start Hoogle with packages in your cabal file.
- Run `treefmt` in nix shell to autoformat the project. This uses treefmt, which uses ./treefmt.toml (where fourmolu and nixpkgs-fmt are specified).
- Run the application without installing: `nix run github:japiirainen/fp` (or `nix run .` from checkout)

`fp` is a programming language heavily inspired by the language John Backus
described in his 1977 Turing Award lecture.

Currently, almost all features described in the paper are implemented. This is not implemented:

- recursion (I'm not sure if I want to allow user defined recursion).

## Credits

- Gabriella Gonzalez's (Gabriella439) [grace](https://github.com/Gabriella439/grace) was an invaluable resource for interpreter design in haskell.
