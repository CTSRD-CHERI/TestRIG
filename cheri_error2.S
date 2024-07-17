# 
#>QCVENGINE_TEST_V2.0
.4byte 0xfe4208db # cgettag x17, x4
#                                                                           ▼        ▼▼▼▼▼▼▼▼▼                ▼▼▼▼▼▼▼▼▼                                                ▼
#      Trap: False, PCWD: 0x0000000080000004, RD: 17, RWD: 0x0000000000000001, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000001, MRM: 0b00000000 I: 0x00000000fe4208db PRV_? XL:? (cgettag x17, x4)
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0x1 != 0x0, mismatch in field rs1_addr: 4 != 0, mismatch in field rs2_addr: 4 != 0, mismatch in field mem_addr: 0x0 != 0xfffffa20000007e0
# ╵
#      Trap: False, PCWD: 0x0000000080000004, RD: 17, RWD: 0x0000000000000000, MA: 0xfffffa20000007e0, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe4208db PRV_? XL:? (cgettag x17, x4)
#                                                                           ▲        ▲▲▲▲▲▲▲      ▲▲          ▲▲▲▲▲▲▲      ▲▲                                          ▲

.4byte 0x04d381ef # jal x3, 231500
#                                                                                            ▼▼▼▼▼                                                              ▼      ▼
#      Trap: False, PCWD: 0x0000000080038850, RD: 03, RWD: 0x0000000080000008, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000080000008, MRM: 0b00000000 I: 0x0000000004d381ef PRV_? XL:? (jal x3, 231500)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 7 != 0, mismatch in field rs2_addr: 13 != 0, mismatch in field mem_addr: 0x0 != 0x80038850
# ╵
#      Trap: False, PCWD: 0x0000000080038850, RD: 03, RWD: 0x0000000080000008, MA: 0x0000000080038850, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x0000000004d381ef PRV_? XL:? (jal x3, 231500)
#                                                                                            ▲  ▲▲▲▲                                                                  ▲▲

.4byte 0x2990a997 # auipc x19, 170250
#                                                                                            ▼▼▼▼▼▼▼                                                            ▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038854, RD: 19, RWD: 0x00000000a9942850, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x00000000a9942850, MRM: 0b00000000 I: 0x000000002990a997 PRV_? XL:? (auipc x19, 170250)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 1 != 0, mismatch in field rs2_addr: 25 != 0, mismatch in field mem_addr: 0x0 != 0xa9942850
# ╵
#      Trap: False, PCWD: 0x0000000080038854, RD: 19, RWD: 0x00000000a9942850, MA: 0x00000000a9942850, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x000000002990a997 PRV_? XL:? (auipc x19, 170250)
#                                                                                            ▲▲▲▲▲▲▲                                                             ▲▲▲▲▲▲▲

.4byte 0xae289be3 # bne x17, x2, -1290
#                                                                                            ▼▼▼▼▼▼           ▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x000000008003834a, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000ae289be3 PRV_? XL:? (bne x17, x2, -1290)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 17 != 0, mismatch in field rs1_rdata: 0x1 != 0x0, mismatch in field rs2_addr: 2 != 0, mismatch in field mem_addr: 0x0 != 0x8003834a
# ╵
#      Trap: False, PCWD: 0x000000008003834a, RD: 00, RWD: 0x0000000000000000, MA: 0x000000008003834a, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000ae289be3 PRV_? XL:? (bne x17, x2, -1290)
#                                                                                            ▲  ▲▲▲▲▲         ▲▲▲▲▲▲▲      ▲▲

.4byte 0x010a7133 # and x2, x20, x16
#                                                            ▼▼▼▼▼▼▼▼▼               ▼▼▼▼▼▼▼▼▼                ▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x000000008003834e, RD: 02, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000010a7133 PRV_? XL:? (and x2, x20, x16)
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0x0 != 0x7ffffd10000003f0, mismatch in field rs1_addr: 20 != 0, mismatch in field rs2_addr: 16 != 0, mismatch in field mem_addr: 0x0 != 0xfffffa20000007e0
# ╵
#      Trap: False, PCWD: 0x000000008003834e, RD: 02, RWD: 0x7ffffd10000003f0, MA: 0xfffffa20000007e0, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000010a7133 PRV_? XL:? (and x2, x20, x16)
#                                                            ▲▲▲▲▲▲▲      ▲▲         ▲▲▲▲▲▲▲      ▲▲          ▲▲▲▲▲▲▲      ▲▲

.4byte 0xfe48815b # cgettag x2, x17
#                                                                                    ▼▼▼▼▼▼▼▼▼                ▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038352, RD: 02, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe48815b PRV_? XL:? (cgettag x2, x17)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 17 != 0, mismatch in field rs1_rdata: 0x1 != 0x0, mismatch in field rs2_addr: 4 != 0, mismatch in field mem_addr: 0x0 != 0x7ffffd10000003f0
# ╵
#      Trap: False, PCWD: 0x0000000080038352, RD: 02, RWD: 0x0000000000000000, MA: 0x7ffffd10000003f0, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe48815b PRV_? XL:? (cgettag x2, x17)
#                                                                                    ▲▲▲▲▲▲▲      ▲▲          ▲▲▲▲▲▲▲      ▲▲

.4byte 0xfe80015b # croundrepresentablelength x2, x0
#                                                            ▼                       ▼▼▼▼▼▼▼▼▼                ▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038356, RD: 02, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe80015b PRV_? XL:? (croundrepresentablelength x2, x0)
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0x0 != 0x8000000000000000, mismatch in field rs2_addr: 8 != 0, mismatch in field mem_addr: 0x0 != 0xfffffa20000007e0
# ╵
#      Trap: False, PCWD: 0x0000000080038356, RD: 02, RWD: 0x8000000000000000, MA: 0xfffffa20000007e0, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe80015b PRV_? XL:? (croundrepresentablelength x2, x0)
#                                                            ▲                       ▲▲▲▲▲▲▲      ▲▲          ▲▲▲▲▲▲▲      ▲▲

.4byte 0xfe2989db # cgetbase x19, x19
#                                                                                    ▼▼▼▼▼▼▼▼▼▼▼▼▼▼           ▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x000000008003835a, RD: 19, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe2989db PRV_? XL:? (cgetbase x19, x19)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 19 != 0, mismatch in field rs1_rdata: 0xa9942850 != 0x0, mismatch in field rs2_addr: 2 != 0, mismatch in field mem_addr: 0x0 != 0x7ffffd10a9942c40
# ╵
#      Trap: False, PCWD: 0x000000008003835a, RD: 19, RWD: 0x0000000000000000, MA: 0x7ffffd10a9942c40, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe2989db PRV_? XL:? (cgetbase x19, x19)
#                                                                                    ▲▲▲▲▲▲▲ ▲▲▲▲▲▲▲          ▲▲▲▲▲▲▲      ▲▲

.4byte 0xabfa0993 # addi x19, x20, -1345
#                                                            ▼▼▼           ▼         ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                                    ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x000000008003835e, RD: 19, RWD: 0xfffffffffffffabf, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xfffffffffffffabf, MRM: 0b00000000 I: 0x00000000abfa0993 PRV_? XL:? (addi x19, x20, -1345)
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0xfffffffffffffabf != 0x7ffffd0ffffffeaf, mismatch in field rs1_addr: 20 != 0, mismatch in field rs2_addr: 31 != 0, mismatch in field mem_addr: 0x0 != 0x7ffffd0ffffffeaf
# ╵
#      Trap: False, PCWD: 0x000000008003835e, RD: 19, RWD: 0x7ffffd0ffffffeaf, MA: 0x7ffffd0ffffffeaf, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000abfa0993 PRV_? XL:? (addi x19, x20, -1345)
#                                                            ▲    ▲▲      ▲          ▲▲▲▲▲▲ ▲▲▲▲▲▲▲▲▲                                                   ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xfe3800db # cgetlen x1, x16
#                                                                                    ▼▼▼▼▼▼▼▼▼                ▼▼      ▼▼▼▼▼▼ ▼                          ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038362, RD: 01, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000080000008, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe3800db PRV_? XL:? (cgetlen x1, x16)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 16 != 0, mismatch in field rs2_addr: 3 != 0, mismatch in field rs2_rdata: 0x80000008 != 0x0, mismatch in field mem_addr: 0x0 != 0xfffffa20000007e0
# ╵
#      Trap: False, PCWD: 0x0000000080038362, RD: 01, RWD: 0xffffffffffffffff, MA: 0xfffffa20000007e0, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe3800db PRV_? XL:? (cgetlen x1, x16)
#                                                                                    ▲▲▲▲▲▲▲      ▲▲          ▲▲▲▲▲▲▲      ▲▲                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xff7088db # cgethigh x17, x1
#                                                                ▼▼▼▼▼▼▼▼            ▼▼▼▼▼▼▼▼▼▼               ▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038366, RD: 17, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000ff7088db PRV_? XL:? (cgethigh x17, x1)
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0x0 != 0x400003fe7ffb, mismatch in field rs1_addr: 1 != 0, mismatch in field rs1_rdata: 0xffffffffffffffff != 0x0, mismatch in field rs2_addr: 23 != 0, mismatch in field mem_addr: 0x0 != 0x7ffffd10000003ef
# ╵
#      Trap: False, PCWD: 0x0000000080038366, RD: 17, RWD: 0x0000400003fe7ffb, MA: 0x7ffffd10000003ef, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000ff7088db PRV_? XL:? (cgethigh x17, x1)
#                                                                ▲    ▲▲▲▲▲▲▲        ▲▲▲▲▲▲▲      ▲▲▲         ▲▲▲▲▲▲▲      ▲▲

.4byte 0xcd580093 # addi x1, x16, -811
#                                                            ▼▼▼▼▼▼▼▼▼    ▼          ▼▼▼▼▼▼▼▼▼                                                          ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x000000008003836a, RD: 01, RWD: 0xfffffffffffffcd5, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xfffffffffffffcd5, MRM: 0b00000000 I: 0x00000000cd580093 PRV_? XL:? (addi x1, x16, -811)
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0xfffffffffffffcd5 != 0x7ffffd10000000c5, mismatch in field rs1_addr: 16 != 0, mismatch in field rs2_addr: 21 != 0, mismatch in field mem_addr: 0x0 != 0x7ffffd10000000c5
# ╵
#      Trap: False, PCWD: 0x000000008003836a, RD: 01, RWD: 0x7ffffd10000000c5, MA: 0x7ffffd10000000c5, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000cd580093 PRV_? XL:? (addi x1, x16, -811)
#                                                            ▲     ▲▲▲▲▲▲▲▲▲         ▲▲▲▲▲▲▲       ▲▲                                                   ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0x241086e3 # beq x1, x1, 2636
#                                                                                            ▼▼▼▼▼▼           ▼▼▼▼▼▼▼▼▼    ▼
#      Trap: False, PCWD: 0x0000000080038db6, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0xfffffffffffffcd5, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000241086e3 PRV_? XL:? (beq x1, x1, 2636)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 1 != 0, mismatch in field rs1_rdata: 0xfffffffffffffcd5 != 0x0, mismatch in field rs2_addr: 1 != 0, mismatch in field rs2_rdata: 0xfffffffffffffcd5 != 0x0, mismatch in field mem_addr: 0x0 != 0x80038db6
# ╵
#      Trap: False, PCWD: 0x0000000080038db6, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000080038db6, MWD: 0x7ffffd10000000c5, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000241086e3 PRV_? XL:? (beq x1, x1, 2636)
#                                                                                            ▲  ▲▲▲▲▲         ▲     ▲▲▲▲▲▲▲▲▲

.4byte 0xfe80895b # croundrepresentablelength x18, x1
#                                                            ▼                       ▼▼▼▼▼▼▼▼▼▼               ▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038dba, RD: 18, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe80895b PRV_? XL:? (croundrepresentablelength x18, x1)
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0x0 != 0x8000000000000000, mismatch in field rs1_addr: 1 != 0, mismatch in field rs1_rdata: 0xfffffffffffffcd5 != 0x0, mismatch in field rs2_addr: 8 != 0, mismatch in field mem_addr: 0x0 != 0xfffffa20000004b5
# ╵
#      Trap: False, PCWD: 0x0000000080038dba, RD: 18, RWD: 0x8000000000000000, MA: 0xfffffa20000004b5, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe80895b PRV_? XL:? (croundrepresentablelength x18, x1)
#                                                            ▲                       ▲▲▲▲▲▲▲      ▲▲▲         ▲▲▲▲▲▲▲      ▲▲

.4byte 0xfe3180db # cgetlen x1, x3
#                                                            ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ ▼           ▼▼▼     ▼▼            ▼    ▼ ▼                          ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038dbe, RD: 01, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000080000008, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe3180db PRV_? XL:? (cgetlen x1, x3)
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0xffffffffffffffff != 0x8080, mismatch in field rs1_addr: 3 != 0, mismatch in field rs1_rdata: 0x80000008 != 0x0, mismatch in field rs2_addr: 3 != 0, mismatch in field rs2_rdata: 0x80000008 != 0x0, mismatch in field mem_addr: 0x0 != 0x7ffffd10800003f8
# ╵
#      Trap: False, PCWD: 0x0000000080038dbe, RD: 01, RWD: 0x0000000000008080, MA: 0x7ffffd10800003f8, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe3180db PRV_? XL:? (cgetlen x1, x3)
#                                                                        ▲ ▲    ▲    ▲▲▲▲▲▲▲ ▲    ▲▲▲▲▲▲▲▲▲▲ ▲▲▲▲▲▲▲▲      ▲▲                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xfe1109db # cgettype x19, x2
#                                                            ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ ▼               ▼   ▼▼         ▼▼▼▼▼                                ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038dc2, RD: 19, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0xffffffffffffffff, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe1109db PRV_? XL:? (cgettype x19, x2)
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0xffffffffffffffff != 0x0, mismatch in field rs1_addr: 2 != 0, mismatch in field rs2_addr: 1 != 0, mismatch in field rs2_rdata: 0xffffffffffffffff != 0x0, mismatch in field mem_addr: 0x0 != 0xfffffd10000003f0
# ╵
#      Trap: False, PCWD: 0x0000000080038dc2, RD: 19, RWD: 0x0000000000000000, MA: 0xfffffd10000003f0, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe1109db PRV_? XL:? (cgettype x19, x2)
#                                                                               ▲         ▲▲▲▲▲▲▲▲▲ ▲▲▲▲▲▲▲▲▲▲▲    ▲▲▲▲▲▲▲▲▲ ▲                          ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xfef9825b # cgetaddr x4, x19
#                                                            ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼ ▼               ▼   ▼▼    ▼                                         ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038dc6, RD: 04, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fef9825b PRV_? XL:? (cgetaddr x4, x19)
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0xffffffffffffffff != 0x0, mismatch in field rs1_addr: 19 != 0, mismatch in field rs1_rdata: 0xffffffffffffffff != 0x0, mismatch in field rs2_addr: 15 != 0, mismatch in field mem_addr: 0x0 != 0x7ffffd10000003f0
# ╵
#      Trap: False, PCWD: 0x0000000080038dc6, RD: 04, RWD: 0x0000000000000000, MA: 0x7ffffd10000003f0, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fef9825b PRV_? XL:? (cgetaddr x4, x19)
#                                                                               ▲    ▲▲▲▲▲▲▲      ▲▲ ▲▲▲▲▲▲▲ ▲▲▲▲▲▲▲▲      ▲▲                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xfe68815b # cgetoffset x2, x17
#                                                                                    ▼▼▼▼▼▼▼▼▼▼▼              ▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038dca, RD: 02, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe68815b PRV_? XL:? (cgetoffset x2, x17)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 17 != 0, mismatch in field rs2_addr: 6 != 0, mismatch in field mem_addr: 0x0 != 0x80003d1003fe83eb
# ╵
#      Trap: False, PCWD: 0x0000000080038dca, RD: 02, RWD: 0x0000000000000000, MA: 0x80003d1003fe83eb, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe68815b PRV_? XL:? (cgetoffset x2, x17)
#                                                                                    ▲   ▲▲▲  ▲▲▲▲▲▲▲         ▲▲▲▲▲▲▲      ▲▲

.4byte 0xfe38085b # cgetlen x16, x16
#                                                                                    ▼▼▼▼▼▼▼▼▼                ▼▼      ▼▼▼▼▼▼ ▼                          ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038dce, RD: 16, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000080000008, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe38085b PRV_? XL:? (cgetlen x16, x16)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 16 != 0, mismatch in field rs2_addr: 3 != 0, mismatch in field rs2_rdata: 0x80000008 != 0x0, mismatch in field mem_addr: 0x0 != 0xfffffa20000007e0
# ╵
#      Trap: False, PCWD: 0x0000000080038dce, RD: 16, RWD: 0xffffffffffffffff, MA: 0xfffffa20000007e0, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe38085b PRV_? XL:? (cgetlen x16, x16)
#                                                                                    ▲▲▲▲▲▲▲      ▲▲          ▲▲▲▲▲▲▲      ▲▲                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xfe6900db # cgetoffset x1, x18
#                                                                                    ▼▼▼▼▼▼▼▼▼                ▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000080038dd2, RD: 01, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe6900db PRV_? XL:? (cgetoffset x1, x18)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 18 != 0, mismatch in field rs2_addr: 6 != 0, mismatch in field mem_addr: 0x0 != 0xfffffd10000003f0
# ╵
#      Trap: False, PCWD: 0x0000000080038dd2, RD: 01, RWD: 0x0000000000000000, MA: 0xfffffd10000003f0, MWD: 0x7ffffd10000003f0, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe6900db PRV_? XL:? (cgetoffset x1, x18)
#                                                                                    ▲▲▲▲▲▲▲      ▲▲          ▲▲▲▲▲▲▲      ▲▲

# Test end
#      halt token
