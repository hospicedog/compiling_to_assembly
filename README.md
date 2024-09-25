# Compiling to Assembly from Scratch
My haskell implementation of the book: Compiling to Assembly from Scratch by Vladimir Keleshev

## Dependencies
- ghc 9.0.0
- nasm
- linux os

## Building

using the Makefile:

```
make
```

This will crate a file on the root directory called "compiler".

Or, manually:

```
cd src
ghc Main.hs
```

## Testing

Testing is done by compiling and running the ".ol" programs in the `test` directory. Use the command:

```
make test
```

This will produce a lot of ".asm" and ".o" files, if you want to remove them:

```
make clean_test
```

Please note that the files will be generated again when if `make test` is called.

