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

We hope that TestRIG will prove fertile ground to inspire innovation in instruction trace generation, counterexample reduction, and model construction such that trace-based verification can approach formal verification for all practical purposes with a much friendlier user experience than either test-suite verification or formal verification currently enable.

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
The RVFI-DII communication uses a single socket with the itrace consumed and the etrace be delivered over the same socket.

Currently, The RVFI-DII interface is known to be implemented by:
- [RVBS](https://github.com/CTSRD-CHERI/RVBS.git)
- [Spike](https://github.com/CTSRD-CHERI/riscv-isa-sim.git)

## Getting started

In order to get the different submodules provided by **TestRIG**, run the following command:

```sh
$ git submodule update --init --recursive
```

Currently, the provided modules are:
- [BSV-RVFI-DII](https://github.com/CTSRD-CHERI/BSV-RVFI-DII.git)
- [RVBS](https://github.com/CTSRD-CHERI/RVBS.git)
- [Spike](https://github.com/CTSRD-CHERI/riscv-isa-sim.git)
- [Sail RISC-V model](https://github.com/rems-project/sail)

The root makefile can currently build the Quick Check Verification Engine, Spike, and the RVBS implementation.
The dependencies for the Quick Check Verification Engine are:
- cabal `sudo apt-get install cabal-install && cabal update`
- Haskell Quick Check verification engine dependencies `cd vengines/QuickCheckVEngine && cabal install --only-dependencies && cd ../..`
The dependencies for Spike are:
- `sudo apt-get install device-tree-compiler`

You can verify a default configuration by executing:
- `make`
- `utils/scripts/runTestRIG.py`

The dependencies for the Sail model can be installed using
[opam](http://opam.ocaml.org/) by following the instructions from the
[Sail wiki](https://github.com/rems-project/sail/wiki/OPAMInstall).
The Sail tool itself will be rebuilt by the module, so you can add
`--deps-only` to the `opam install` command to avoid building it
twice.
