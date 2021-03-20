# bottom-hs

Based off of the hilarious [bottom-rs](https://github.com/bottom-software-foundation/bottom-rs), built to the [Bottom spec](https://github.com/bottom-software-foundation/spec).

![,,,,,,,,,](https://cdn.discordapp.com/attachments/644479051918082050/799905088541425664/bottom.jpg)

## Usage

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

## Development

- [ ] Add microbenchmarks.

## License

Apache 2.
