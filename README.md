# bottom-hs

[![Build and test](https://github.com/bottom-software-foundation/bottom-hs/actions/workflows/main.yml/badge.svg)](https://github.com/bottom-software-foundation/bottom-hs/actions/workflows/main.yml)
[![Hackage](https://img.shields.io/hackage/v/bottom)](https://hackage.haskell.org/package/bottom)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/bottom)](https://packdeps.haskellers.com/feed?needle=bottom)

Based off of the hilarious [bottom-rs](https://github.com/bottom-software-foundation/bottom-rs), built to the [Bottom spec](https://github.com/bottom-software-foundation/spec).

![,,,,,,,,,](https://cdn.discordapp.com/attachments/644479051918082050/799905088541425664/bottom.jpg)

## Using the CLI

```sh
$ cabal run bottom -- --help

Bottom translator 0.1.0

Usage: bottom ((-b|--bottomify) | (-r|--regress) | (-V|--version)) <TEXT>
  Fantastic (maybe) CLI for translating between bottom and human-readable text

Available options:
  -b,--bottomify           Translate text to bottom
  -r,--regress             Translate bottom to human-readable text (futile)
  -V,--version             Prints version information
  -h,--help                Show this help text
```
```sh
$ cabal run bottom -- --regress 🫂✨✨🥺,,👉👈💖💖✨✨🥺,,,,👉👈💖💖✨✨✨✨👉👈🫂✨✨🥺,,👉👈💖💖✨✨✨👉👈💖💖✨✨✨✨🥺,,👉👈🫂✨✨🥺,,👉👈💖💖✨✨🥺,,,,👉👈💖💖💖✨✨🥺,👉👈🫂✨✨🥺,,👉👈💖💖✨✨✨👉👈 💖💖✨✨✨✨👉👈
がんばれ
```
```sh
$ cabal run bottom -- --bottomify Test
💖✨✨✨,,,,👉👈💖💖,👉👈💖💖✨🥺👉👈💖💖✨🥺,👉👈
```

## Usage as a library

See the generated Haddock documentation in Hackage for module documentation.

## Development

Useful commands:

- `cabal run bottom -- --help`: Runs the CLI.
- `cabal repl`: Run the library in the REPL. Loads `Data.Encoding.Bottom` with `-XOverloadedStrings` by default (see `.ghci`).
- `cabal test --enable-coverage`: Run tests, with test coverage.
- `cabal bench`: Run benchmarks.
- `cabal haddock --haddock-all --enable-documentation`: Generate documentation.
- `ormolu --mode inplace $(find . -path ./dist-newstyle -prune -false -o -type f -name '*.hs')`: Format code (see [`ormolu`](https://github.com/tweag/ormolu)).
- `cabal-fmt -i bottom.cabal`: Format `.cabal` file (see [`cabal-fmt`](https://github.com/phadej/cabal-fmt)).

## License

Apache 2.
