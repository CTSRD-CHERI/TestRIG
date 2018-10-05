# TestRIG
Framework for testing RISC-V processors with Random Instruction Generation.

## Glorious vision
TestRIG is a framework for RISC-V processor verification using the RVFI-DII (pronounced "rividy") interface.
TestRIG supports two types of components:
1. Vengines (verification engines)
2. Implementations (including Models)
Vengines generate RVFI-DII instruction traces, and consume RVFI-DII exeuction traces.
Implemenations consume RVFI-DII instruction traces, and generate RVFI-DII execution traces.

### Vengines
The vengines include instruction trace (itrace) generators (targetting various classes of behaviour).
The vengine then feeds these itraces into both a model and an implementation through two TCP sockets in the RVFI-DII format.
The model and implementation return an RVFI-DII execution trace (etrace) that details the state change due to each instruction.
The vengine compares these two traces and identifies any divergence between the model and the implementation.
Any failure is reported nicely to the user with a means of conveniently replaying the failing itrace for debugging.
A capable vengine will likely also attempt to reduce the failing trace to a minimal example that diverges.

### Implementations (including Models)
Any implementation that wants to be verified using TestRIG will need to support RVFI-DII itrace format as an instruction source and etrace reporting format from a socket.
The implementation should have a mode where instructions are consumed exlusively from the RVFI-DII instruction port, bypassing any actual instruction memory, ignoring the architectural program counter.
An implementation should then deliver a trace report at the end of execution detailing implementation behaviour in response to that instruction in the RVFI-DII format.

## Getting started

In order to get the different submodules provided by **TestRIG**, run the following command:

```sh
$ git submodule update --init --recursive
```

Currently, the provided modules are:
- [BSV-RVFI-DII](https://github.com/CTSRD-CHERI/BSV-RVFI-DII.git)
