# TestRIG
Framework for testing RISC-V processors with Random Instruction Generation.

## Glorious vision
TestRIG is a framework for RISC-V processor verification using the RVFI-DII (pronounced "rividy") interface.
TestRIG supports two types of components:

1. Vengines (verification engines)
2. Implementations (including Models)

Vengines generate two equivelant streams of RVFI-DII instruction traces, and consume two streams of RVFI-DII exeuction traces, asserting that they are equivelant.
Implemenations consume RVFI-DII instruction traces, and generate RVFI-DII execution traces.

TestRIG eliminates the "test gap" between specification and implementation.
If the specification is an executable model (see Sail, L3, and many other efforts), then TestRIG allows automated verification of any specified property without passing through human interpretation and hand-writing tests.
While TestRIG should require work to construct trace generators for classes of ISA behaviour, new instructions in a class can automatically be included in new traces and will be run in many more variations than hand-written tests would allow.

TestRIG verifies the pipeline as well as specification compliance.
Test suites are designed to test as many aspects of an instruction set specification as possible, but cannot be expected to provide any reasonable verification of complex pipeline behaviour.
TestRIG can verifiy every register value read in the pipeline under random sequence generation, while a test suite will only report a prescribed test result.

TestRIG greatly increases debugging efficiency.
In-memory test suites require a significant amount of boiler plate in order to construct a valid test state which cannot be automatically reduced without disturbing instruction layout in complex ways.
As TestRIG relies on direct instruction injection, bypassing fetch through PC, a sequence of instructions can easily be shortened by simply eliminating instructions from the trace to see if we still find divergence.
As a result we can expect automatically reduced counterexamples on the order of a handful of instructions.

We hope that TestRIG will prove fertile ground to inspire innovation in instruction trace generation, counterexample reduction, and model construction such that trace-based verification can approach formal verification for many practical purposes with a much friendlier user experience than either test-suite verification or formal verification currently enable.

## Vengines
A vengine typically includes instruction trace (itrace) generators targetting various classes of behaviour.
The vengine then feeds these itraces into both a model and an implementation through two TCP sockets in the RVFI-DII format.
The model and implementation return an RVFI-DII execution trace (etrace) that details the state observation and state change of each instruction.
The vengine compares these two traces and identifies any divergence between the model and the implementation.
Any failure is reported nicely to the user with a means of conveniently replaying the failing itrace for debugging.
A capable vengine will also attempt to reduce the failing trace to a minimal example that diverges.

## Implementations (including Models)
Any implementation that wants to be verified using TestRIG will need to support RVFI-DII itrace format as an instruction source and etrace reporting format.
The implementation should have a mode where instructions are consumed exlusively from the RVFI-DII instruction port, bypassing any actual instruction memory, ignoring the architectural program counter.
An implementation should then deliver a trace report at the end of execution detailing implementation behaviour in response to that instruction in the RVFI-DII format.
The RVFI-DII communication uses a single socket with the itrace consumed and the etrace be delivered over the same socket. Please look at [the RVFI-DII](https://github.com/CTSRD-CHERI/TestRIG/blob/master/RVFI-DII.md) specification for more details

Currently, the provided modules are:
- [BSV-RVFI-DII](https://github.com/CTSRD-CHERI/BSV-RVFI-DII.git)
- [RVBS](https://github.com/CTSRD-CHERI/RVBS.git)
- [CHERI Spike](https://github.com/CTSRD-CHERI/riscv-isa-sim.git)
- [Sail RISC-V model](https://github.com/rems-project/sail-riscv.git)
- [CHERI Sail RISC-V model](https://github.com/CTSRD-CHERI/sail-cheri-riscv.git)
- [Piccolo](https://github.com/CTSRD-CHERI/Piccolo.git)
- [Flute](https://github.com/CTSRD-CHERI/Flute.git)
- [Toooba](https://github.com/CTSRD-CHERI/Toooba.git)
- [Ibex](https://github.com/CTSRD-CHERI/ibex.git)
- [QEMU](https://github.com/CTSRD-CHERI/qemu.git)

## Getting started

In order to get the different submodules provided by **TestRIG**, run the following command:

```sh
$ git submodule update --init --recursive
```

The root makefile can currently build the QuickCheck Verification Engine, Spike, and the Sail implementation. Both Spike and Sail are built without CHERI support by default and Sail is built as a 32-bit version.

### Dependencies
The dependencies for the QuickCheck Verification Engine are:
- cabal `sudo apt-get install cabal-install && cabal update`
- The Haskell modules dependencies `cd vengines/QuickCheckVEngine && cabal install --only-dependencies && cd ../..`

The dependencies for Spike are:
- `sudo apt-get install device-tree-compiler`

The dependencies for RVBS are the Bluespec compiler `bsc`. For people on the University of Cambridge, Computer Laboratory's internal network; execute the following command. Alternatively, you can follow the build instructions on the [B-Lang-org github](https://github.com/B-Lang-org/bsc).
- `source /usr/groups/ecad/setup.bash`

The dependencies for the Sail model can be installed using
[opam](http://opam.ocaml.org/) by following the instructions from the
[Sail wiki](https://github.com/rems-project/sail/blob/sail2/INSTALL.md). You may have to run `opam update` and `opam upgrade` occasionally if sail gets updated.

The dependencies for Ibex are verilator:
- `sudo apt-get install verilator`

Toooba depends on the Bluespec compiler (see the dependencies for RVBS) and verilator. Toooba needs the version of verilator to be greater than: `Verilator 3.922 2018-03-17 rev verilator_3_920-32-gdf3d1a4`

### Default Configuration

You can verify a default configuration by executing:
```sh
$ make
$ utils/scripts/runTestRIG.py
```

## Custom Configurations
Look at the `Makefile` to see different targets to compare against each other. Also use the following command to figure out the different options for running TestRIG:

```sh
$ utils/scripts/runTestRIG.py --help
```

### CHERI 64-bit: Sail vs Spike
Executing the following commands will compare Sail and Spike with the 64-bit version of the RISC-V instruction set and CHERI extensions enabled, assuming that you've initialized the sumblodules and have installed all the dependencies described above.
```sh
$ make
$ make sail-rv64-cheri
$ make spike-cheri
$ utils/scripts/runTestRIG.py -a sail -b spike -r rv64ixcheri
```

### CHERI 32-bit: Sail vs Ibex
Executing the following commands will compare Sail and Ibex with the 32-bit version of the RISC-V instruction set (including compressed) and CHERI extensions enabled, assuming that you've initialized the sumblodules and have installed all the dependencies described above.
```sh
$ make
$ make sail-rv32-cheri
$ make ibex-rv32ic-cheri
$ utils/scripts/runTestRIG.py -a sail -b ibex -r rv32icxcheri
```
