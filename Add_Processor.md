# How to add a new processor to TestRIG

This guide will show the first steps to add a processor other than the ones already listed in the TestRIG repository. It will explain what files to change and how to use the RVFI-DII protocol to interact with the new processor. There is a [repository](https://github.com/GabPGomes/TestRIG) with the complete example that may help in applying the guide.

### Example processor to be added and simulation tool

The processor to be added will be the [tinyriscv](https://github.com/liangkangnan/tinyriscv). It is a very simple and easy to understand core so there won't be many processor-specific challenges to add it. This will keep the focus of this guide on the steps common to all processors.

The simulation engine will be [cocotb](https://www.cocotb.org/) together with [Icarus Verilog](https://steveicarus.github.io/iverilog/). Cocotb is a Python verification framework that allows the user to interact with a RTL project through simulation while using python code instead of HDL. This gives the user access to python tools (such as the socket library) that otherwise would be harder to get using HDLs.

## First Step: dependencies and clone tinyriscv

First install the needed dependencies from TestRIG README before continuing. Once done, clone the fork of tinyriscv processor in the riscv-implementations folder.

```sh
cd riscv-implementations
git submodule add https://github.com/GabPGomes/tinyriscv
```

Install cocotb using a virtual environment
```sh
python3 -m venv venv
source venv/bin/activate
pip install cocotb
```

Install Icarus verilog v11 (stable) using apt or other package manager.
```sh
sudo apt install iverilog
```

This step is in [this commit](https://github.com/GabPGomes/TestRIG/commit/0241a7281e551757e83b4b3707579473abaacfbb), in the example repository.

## Second Step: add tinyriscv to the `runTestRIG.py` file

All the following changes are in this [commit](https://github.com/GabPGomes/TestRIG/commit/b440eaff7130c93af443c018a07e50df52a1497a)

Add Tiyriscv to the known implementations:
```python
known_rvfi_dii = {'spike', 'rvbs', 'sail', 'piccolo', 'flute', 'toooba', 'ibex', 'muntjac', 'qemu', 'tinyriscv', 'manual', 'none'}
  ```

Add the path to the new processor
```python
parser.add_argument('--path-to-tinyriscv', metavar='PATH', type=str,
  default=op.join(implementations_path, "tinyriscv/cocotb/run_tinyriscv.py"),
  help="The PATH to the tinyriscv run script")
```

Add the command to run the simulation
```python
  elif name == 'tinyriscv':
    cmd = ['python3', args.path_to_tinyriscv, str(port)]
```

It is important to pass the port as an argument because the socket port is different at each execution. The IP is always 127.0.0.1 (localhost).


## Third Step: adding cocotb files

Copy the script in a new `cocotb` folder to run the cocotb simulation `run_tinyriscv.py`. It calls the cocotb simulation using the `make` command and passes the port as a variable.

Now the cocotb files `cocotb_test.py`, `socket_interface.py`, `simulation_helpers.py` will be added. The makefile is added here, too.

The main points of the code will be explained, but the files can be seen in this [commit](https://github.com/GabPGomes/tinyriscv/commit/6019316e68ba06be1c2c8c9764744c7352e52a55).

### socket_interface.py

This code is responsible for the socket communication and the most important part of it is the `rvfi_dii_init` function. It follows the TestRIG protocol to initiate the verification as shown in the figure below:

![first_communication](first_communication.svg)

1. The socket connection is established with the tinyriscv implementation as the server and TestRIG as the client;
2. TestRIG sends an instruction packet where the field insn='V''E''R''S' to negotiate the version
3. The tinyriscv simulation must respond with a halt=1 packet. If you choose to set the halt field with a number bigger than one, version 2 trace negotiation will begin (not in the scope of this guide).
4. TestRIG will then start sending the instructions of the tests. Each test ends with an End of Trace command (cmd=0).
5. The tinyriscv simulation must respond the packets with the corresponding execution trace and, after each End of Trace, respond with a halt packet and reset the processor and memory.

### Simulation

The other two files are responsible to guide the simulation. Since tinyriscv is simple, all that is needed is to put the next instruction at the instruction input port `rib_pc_data_i` and run the clock. The relevant signals are gathered by the `pipeline_monitor` as the instruction goes through the pipeline. The signals are used to fill the fields of each execution trace packet.

One of the requirements of the simulation is to set all of the register file to zero before starting the verification.

```python
    for reg in dut.u_regs.regs:
        reg.value = 0
``` 

## Fourth step: Change CPU Reset Address

Change tinyriscv CpuResetAddr to 0x80000000 in the `defines.v` file to follow TestRIG specification.

```verilog
`define CpuResetAddr = 32'h80000000
```
This last change is in this [commit](https://github.com/GabPGomes/tinyriscv/commit/d636d899ec9171eaa8968048319fec431bb99abb)

## Fifth step: Run Arithmetic Tests Only

The command to run the arithmetic tests tinyriscv against sail.

```sh
make vengines
make sail-rv32
python3 utils/scripts/runTestRIG.py -a tinyriscv -b sail -r rv32i --test-include-regex "arith"
```