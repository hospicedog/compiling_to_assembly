# Compiling to Assembly from Scratch
My haskell implementation of the book Compiling to Assembly from Scratch by Vladimir Keleshev

## Dependencies
- ghc 9.0.0
- linux os

## Building

```
$ cd src
$ ghc compiler.hs
```

Alternatively, to avoid cding and generating lots of .o and .hi files

```
ghc -Isrc -no-keep-o-files -no-keep-hi-files -o compiler src/compiler.hs
```
