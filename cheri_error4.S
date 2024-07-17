# 
#>QCVENGINE_TEST_V2.0
.4byte 0xff780a5b # cgethigh x20, x16
#                                                                                                                                                       ▼▼▼▼
#      Trap: False, PCWD: 0x0000000080000004, RD: 20, RWD: 0xffff000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffff000000000000, MRM: 0b00000000 I: 0x00000000ff780a5b PRV_? XL:? (cgethigh x20, x16)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 16 != 0, mismatch in field rs2_addr: 23 != 0
# ╵
#      Trap: False, PCWD: 0x0000000080000004, RD: 20, RWD: 0xffff000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000ff780a5b PRV_? XL:? (cgethigh x20, x16)
#                                                                                                                                                                   ▲▲▲▲

.4byte 0xfe10005b # cgettype x0, x0
#                                                                                                                                                       ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080000008, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe10005b PRV_? XL:? (cgettype x0, x0)
# ╷
# │ ^ A, B v: mismatch in field rs2_addr: 1 != 0
# ╵
#      Trap: False, PCWD: 0x0000000080000008, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe10005b PRV_? XL:? (cgettype x0, x0)
#                                                                                                                                                       ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0x30200073 # mret
#                                   ▼▼
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x0000000030200073 PRV_? XL:? (mret)
# ╷
# │ ^ A, B v: mismatch in field rs2_addr: 2 != 0, mismatch in field pc_wdata: 0x0 != 0x8000000c
# ╵
#      Trap: False, PCWD: 0x000000008000000c, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x0000000030200073 PRV_? XL:? (mret)
#                                   ▲      ▲

.4byte 0x881097e3 # bne x1, x1, -1906
#            ▼▼▼▼                          ▼                                                                                                                                                           ▼▼▼ ▼ ▼              ▼▼ ▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000000000004, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000881097e3 PRV_? XL:? (bne x1, x1, -1906)
# ╷
# │ ^ A, B v: mismatch in field insn: 0x881097e3 != 0x30200073, mismatch in field trap: 0 != 1, mismatch in field rs1_addr: 1 != 0, mismatch in field rs2_addr: 1 != 0, mismatch in field pc_wdata: 0x4 != 0x0
# ╵
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x0000000030200073 PRV_? XL:? (mret)
#            ▲▲▲▲                          ▲                                                                                                                                                           ▲ ▲▲▲▲               ▲▲ ▲

.4byte 0xfe78825b # cgetflags x4, x17
#                                          ▼
#      Trap: False, PCWD: 0x0000000000000008, RD: 04, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe78825b PRV_? XL:? (cgetflags x4, x17)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 17 != 0, mismatch in field rs2_addr: 7 != 0, mismatch in field pc_wdata: 0x8 != 0x4
# ╵
#      Trap: False, PCWD: 0x0000000000000004, RD: 04, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe78825b PRV_? XL:? (cgetflags x4, x17)
#                                          ▲

.4byte 0xfe38805b # cgetlen x0, x17
#                                          ▼                                                                                                            ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x000000000000000c, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe38805b PRV_? XL:? (cgetlen x0, x17)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 17 != 0, mismatch in field rs2_addr: 3 != 0, mismatch in field pc_wdata: 0xc != 0x8
# ╵
#      Trap: False, PCWD: 0x0000000000000008, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe38805b PRV_? XL:? (cgetlen x0, x17)
#                                          ▲                                                                                                            ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0x001930b3 # sltu x1, x18, x1
#                                         ▼
#      Trap: False, PCWD: 0x0000000000000010, RD: 01, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000001930b3 PRV_? XL:? (sltu x1, x18, x1)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 18 != 0, mismatch in field rs2_addr: 1 != 0, mismatch in field pc_wdata: 0x10 != 0xc
# ╵
#      Trap: False, PCWD: 0x000000000000000c, RD: 01, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000001930b3 PRV_? XL:? (sltu x1, x18, x1)
#                                          ▲

.4byte 0xfe990a5b # crepresentablealignmentmask x20, x18
#                                          ▼                                                                                                            ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000000000014, RD: 20, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe990a5b PRV_? XL:? (crepresentablealignmentmask x20, x18)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 18 != 0, mismatch in field rs2_addr: 9 != 0, mismatch in field pc_wdata: 0x14 != 0x10
# ╵
#      Trap: False, PCWD: 0x0000000000000010, RD: 20, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe990a5b PRV_? XL:? (crepresentablealignmentmask x20, x18)
#                                          ▲                                                                                                            ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xfe01025b # cgetperm x4, x2
#                                          ▼                                                                                                                       ▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000000000018, RD: 04, RWD: 0x0000000000078fff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000078fff, MRM: 0b00000000 I: 0x00000000fe01025b PRV_? XL:? (cgetperm x4, x2)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 2 != 0, mismatch in field pc_wdata: 0x18 != 0x14
# ╵
#      Trap: False, PCWD: 0x0000000000000014, RD: 04, RWD: 0x0000000000078fff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe01025b PRV_? XL:? (cgetperm x4, x2)
#                                          ▲                                                                                                                       ▲▲▲▲▲

.4byte 0xac48a4a3 # sw x4, x17[-1335]
#                                                                                                                                          ▼▼▼▼
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0xfffffffffffffac9, MWD: 0x0000000000078fff, MWM: 0b00001111, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000ac48a4a3 PRV_? XL:? (sw x4, x17[-1335])
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 17 != 0, mismatch in field rs2_addr: 4 != 0, mismatch in field rs2_rdata: 0x78fff != 0x0, mismatch in field mem_wdata: 
# ╵
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0xfffffffffffffac9, MWD: 0x0000000000078fff, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000ac48a4a3 PRV_? XL:? (sw x4, x17[-1335])
#                                                                                                                                          ▲▲▲▲

.4byte 0xfe5a00db # cgetsealed x1, x20
#                                                                                    ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000000000004, RD: 01, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe5a00db PRV_? XL:? (cgetsealed x1, x20)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 20 != 0, mismatch in field rs1_rdata: 0xffffffffffffffff != 0x0, mismatch in field rs2_addr: 5 != 0, mismatch in field mem_addr: 0x0 != 0xffffffffffffffff
# ╵
#      Trap: False, PCWD: 0x0000000000000004, RD: 01, RWD: 0x0000000000000000, MA: 0xffffffffffffffff, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe5a00db PRV_? XL:? (cgetsealed x1, x20)
#                                                                                    ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xfe11095b # cgettype x18, x2
#                                                                                                                                                       ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000000000008, RD: 18, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe11095b PRV_? XL:? (cgettype x18, x2)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 2 != 0, mismatch in field rs2_addr: 1 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000008, RD: 18, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe11095b PRV_? XL:? (cgettype x18, x2)
#                                                                                                                                                       ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xfe28095b # cgetbase x18, x16
#      Trap: False, PCWD: 0x000000000000000c, RD: 18, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe28095b PRV_? XL:? (cgetbase x18, x16)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 16 != 0, mismatch in field rs2_addr: 2 != 0
# ╵
#      Trap: False, PCWD: 0x000000000000000c, RD: 18, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe28095b PRV_? XL:? (cgetbase x18, x16)

.4byte 0xfe39015b # cgetlen x2, x18
#                                                                                                                                                       ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000000000010, RD: 02, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe39015b PRV_? XL:? (cgetlen x2, x18)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 18 != 0, mismatch in field rs2_addr: 3 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000010, RD: 02, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe39015b PRV_? XL:? (cgetlen x2, x18)
#                                                                                                                                                       ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0x0130a133 # slt x2, x1, x19
#      Trap: False, PCWD: 0x0000000000000014, RD: 02, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x000000000130a133 PRV_? XL:? (slt x2, x1, x19)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 1 != 0, mismatch in field rs2_addr: 19 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000014, RD: 02, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x000000000130a133 PRV_? XL:? (slt x2, x1, x19)

.4byte 0x87389883 # lh x17, x17[-1933]
#                                                            ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                                                           ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼               ▼▼
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0xaaaaaaaaaaaaaaaa, MA: 0xfffffffffffff873, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xaaaaaaaaaaaaaaaa, MRM: 0b00000011 I: 0x0000000087389883 PRV_? XL:? (lh x17, x17[-1933])
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0xaaaaaaaaaaaaaaaa != 0x0, mismatch in field rs1_addr: 17 != 0, mismatch in field rs2_addr: 19 != 0, mismatch in field mem_rdata: 
# ╵
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0xfffffffffffff873, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x0000000087389883 PRV_? XL:? (lh x17, x17[-1933])
#                                                            ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                                                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲               ▲▲

.4byte 0xfe19005b # cgettype x0, x18
#                                                                                                                                                       ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000000000004, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe19005b PRV_? XL:? (cgettype x0, x18)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 18 != 0, mismatch in field rs2_addr: 1 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000004, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe19005b PRV_? XL:? (cgettype x0, x18)
#                                                                                                                                                       ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xfe28815b # cgetbase x2, x17
#      Trap: False, PCWD: 0x0000000000000008, RD: 02, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe28815b PRV_? XL:? (cgetbase x2, x17)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 17 != 0, mismatch in field rs2_addr: 2 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000008, RD: 02, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe28815b PRV_? XL:? (cgetbase x2, x17)

.4byte 0x7340aca3 # sw x20, x1[1849]
#                                                                                                             ▼▼▼▼▼▼▼▼                     ▼▼▼▼
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000739, MWD: 0xffffffffffffffff, MWM: 0b00001111, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x000000007340aca3 PRV_? XL:? (sw x20, x1[1849])
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 1 != 0, mismatch in field rs2_addr: 20 != 0, mismatch in field rs2_rdata: 0xffffffffffffffff != 0x0, mismatch in field mem_wdata: 
# ╵
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000739, MWD: 0x00000000ffffffff, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x000000007340aca3 PRV_? XL:? (sw x20, x1[1849])
#                                                                                                             ▲▲▲▲▲▲▲▲                     ▲▲▲▲

.4byte 0xfe2880db # cgetbase x1, x17
#      Trap: False, PCWD: 0x0000000000000004, RD: 01, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe2880db PRV_? XL:? (cgetbase x1, x17)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 17 != 0, mismatch in field rs2_addr: 2 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000004, RD: 01, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe2880db PRV_? XL:? (cgetbase x1, x17)

# Test end
#      halt token
