# QuickCheckVEngine

**QCVEngine** is a very basic QuickCheck-based random instruction generator
for RISC-V (currently the RV32I subset). The main feature of the generator is a
small library that makes it easy to define instruction encoders and decoders.
Why not [have a look](src/RISCV.hs)?

## Getting started

You should be able to build QCVEngine using cabal. Simply run
```sh
$ cabal configure
$ cabal build
$ cabal install
```
and the `QCVEngine` binary should be in your `~/.cabal/bin`.
