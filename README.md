## Table of contents

- [Table of contents](#table-of-contents)
- [fp programming language](#fp-programming-language)
- [Examples of `fp`](#examples-of-fp)
- [Usage](#usage)
  - [Command line](#command-line)
  - [Interpret](#interpret)
  - [REPL](#repl)
- [Development](#development)
  - [Nix support](#nix-support)
  - [Tips](#tips)

## fp programming language

`fp` is a programming language heavily inspired by the language John Backus
described in his 1977 Turing Award lecture.

The paper can be found [here](https://dl.acm.org/doi/10.1145/359576.359579).

## Examples of `fp`

```haskell
Def Innerproduct ≡ (Insert +)∘(ApplyToAll *)∘Transpose

-- or abbreviated
Def IP ≡ (/+)∘(α*)∘Trans
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
Def IP ≡ (/+)∘(α*)∘Trans

IP:<<1,2,3>,<6,5,4>>
```

This program lives in `examples/IP.fp` and can be interpreted like this.

```haskell
cabal run fp -- interpret examples/IP.fp
```

Which will yield `Atom (Int 28)`.

### REPL

not implemented yet!

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

Currently there is enough of an implementation to interpret the first example given by Backus in his paper. This program is calculating the 'inner product'.