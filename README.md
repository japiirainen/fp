## fp programming language

`fp` is a programming language heavily inspired by the language John Backus
described in his 1977 Turing Award lecture.


## Usage of `fp`


You can also run the test suite.

```sh
cabal test tasty
```

## Nix support

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

## Tips

- Run `nix flake update` to update all flake inputs.
- Run `./bin/hoogle` to start Hoogle with packages in your cabal file.
- Run `treefmt` in nix shell to autoformat the project. This uses treefmt, which uses ./treefmt.toml (where fourmolu and nixpkgs-fmt are specified).
- Run the application without installing: `nix run github:japiirainen/fp` (or `nix run .` from checkout)
