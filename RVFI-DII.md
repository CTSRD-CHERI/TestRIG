# RVFI-DII Specification

RVFI-DII (Risc-V Formal Interface - Direct Instruction Injection) is based on RVFI from
Clifford Wolf used for formal verification of Risc-V implementations. It adds an instruction
trace format and standardises a packet format for the execution trace.

RVFI-DII is composed of two packet structures designed to send across sockets.
 * The **instruction trace** format that is sent from the vengine to the implementation
 * The **execution trace** format that is reported from the implementation to the vengine

## RVFI-DII Instruction Packet (8 bytes)
 ~~~

struct RVFI_DII_Instruction_Packet {
   Bit8  padding;        // [7]
   Bit8  rvfi_cmd;       // [6] This token is a trace command.  For example, reset device under test.
   Bit16 rvfi_time;      // [5 - 4] Time to inject token.  The difference between this and the previous
                   // instruction time gives a delay before injecting this instruction.
                   // This can be ignored for models but gives repeatability for implementations
                   // while shortening counterexamples.
   Bit32 rvfi_insn;      // [0 - 3] Instruction word: 32-bit instruction or command. The lower 16-bits
                   // may decode to a 16-bit compressed instruction.
}
~~~

The **rvfi_cmd** field currently has two values defined:  
0 = **EndOfTrace** // Reset the implemenation, including registers, memory, and PC (to 0x80000000)  
1 = **Instruction** // Execute the instruction in rvfi_insn  

## RVFI-DII Execution Packet (88 bytes)
~~~
struct RVFI_DII_Execution_Packet {
   Bit8  rvfi_intr;      // [87] Trap handler:            Set for first instruction in trap handler.
   Bit8  rvfi_halt;      // [86] Halt indicator:          Marks the last instruction retired 
                            //                                      before halting execution.
   Bit8  rvfi_trap;      // [85] Trap indicator:          Invalid decode, misaligned access or
                            //                                      jump command to misaligned address.
   Bit8  rvfi_rd_addr;   // [84]      Write register address:  MUST be 0 if not used.
   Bit8  rvfi_rs2_addr;  // [83]                          otherwise set as decoded.
   Bit8  rvfi_rs1_addr;  // [82]      Read register addresses: Can be arbitrary when not used,
   Bit8  rvfi_mem_wmask; // [81]      Write mask:              Indicates valid bytes written. 0 if unused.
   Bit8  rvfi_mem_rmask; // [80]      Read mask:               Indicates valid bytes read. 0 if unused.
   Bit64 rvfi_mem_wdata; // [72 - 79] Write data:              Data written to memory by this command.
   Bit64 rvfi_mem_rdata; // [64 - 71] Read data:               Data read from mem_addr (i.e. before write)
   Bit64 rvfi_mem_addr;  // [56 - 63] Memory access addr:      Points to byte address (aligned if define
                            //                                      is set). *Should* be straightforward.
   Bit64 rvfi_rd_wdata;  // [48 - 55] Write register value:    MUST be 0 if rd_ is 0.
   Bit64 rvfi_rs2_data;  // [40 - 47]                          above. Must be 0 if register ID is 0.
   Bit64 rvfi_rs1_data;  // [32 - 39] Read register values:    Values as read from registers named
   Bit64 rvfi_insn;      // [24 - 31] Instruction word:        32-bit command value.
   Bit64 rvfi_pc_wdata;  // [16 - 23] PC after instr:          Following PC - either PC + 4 or jump target.
   Bit64 rvfi_pc_rdata;  // [08 - 15] PC before instr:         PC for current instruction
   Bit64 rvfi_order;     // [00 - 07] Instruction number:      INSTRET value after completion.
}
~~~

# Other TestRIG Requirements

TestRIG efficiently verifies a measure of equivelance between a model and an implementation at the cost of constructing
a test harness for the design.
Specifically:

## RVFI Trace Reporting
The records for the execution trace format must be preserved to the end of the pipeline and reported over the RVFI-DII
socket interface.
This requirement is very similar to standard RVFI support recommended by Clifford Wolf, but standardises a socket interface
for interoperability between verification tools.

## Direct Instruction Injection
An implementation that can be verified using TestRIG must support direct instruction injection.
That is, any ICache and branch prediction mechanism must be bypassed, and instructions should be consumed
from the RVFI-DII socket interface.
The Bluespec HDL example receives instruction traces from the RVFI-DII socket and fills a FIFO until it receives
an **EndOfTrace** token, indicating that the FIFO holds a complete instruction trace.
It then begins injecting instructions when a 16-bit counter is equal to the value in the **rvfi_time** field.
This preserves timing relationships between instructions even as the vengine eliminates intervening instructions.

## TestRIG-triggered Reset
The design must reset on each **EndOfTrace** command from the RVFI-DII instruction trace socket.
That is, all general-purpose register values must be set to zero, CSRs to reset value, and memory to zero.

## 64KiB Memory at 0x80000000
Memory must be 64KiB in size and mapped at 0x80000000.
Test generators are responsable to attempt to generate addresses within this 64KiB region.
