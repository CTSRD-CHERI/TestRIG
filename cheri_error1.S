# 
#>QCVENGINE_TEST_V2.0
.4byte 0x0bc90013 # addi x0, x18, 188
#                                   ▼      ▼                                                                                                                          ▼▼                                 ▼▼▼▼▼▼              ▼▼▼▼▼▼▼   ▼   ▼▼
#      Trap: False, PCWD: 0x0000000080000004, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x00000000000000bc, MRM: 0b00000000 I: 0x000000000bc90013 PRV_? XL:? (addi x0, x18, 188)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xbc90013 != 0xfe7880db, mismatch in field rs1_addr: 18 != 0, mismatch in field rs2_addr: 28 != 0, mismatch in field pc_wdata: 0x80000004 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                         ▲▲                                                                                                                          ▲▲                               ▲▲▲▲▲ ▲              ▲▲▲▲▲▲ ▲▲     ▲ ▲

.4byte 0xfe7880db # cgetflags x1, x17
#                                   ▼      ▼       ▼
#      Trap: False, PCWD: 0x0000000080000008, RD: 01, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
# ╷
# │ ^ A, B v: mismatch in field rd_addr: 1 != 0, mismatch in field rs1_addr: 17 != 0, mismatch in field rs2_addr: 7 != 0, mismatch in field pc_wdata: 0x80000008 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                         ▲▲       ▲

.4byte 0xfef10a5b # cgetaddr x20, x2
#                                   ▼      ▼      ▼                                                                                                                                                      ▼▼ ▼▼                   ▼▼▼  ▼▼   ▼
#      Trap: False, PCWD: 0x000000008000000c, RD: 20, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fef10a5b PRV_? XL:? (cgetaddr x20, x2)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfef10a5b != 0xfe7880db, mismatch in field rd_addr: 20 != 0, mismatch in field rs1_addr: 2 != 0, mismatch in field rs2_addr: 15 != 0, mismatch in field pc_wdata: 0x8000000c != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                         ▲▲       ▲                                                                                                                                                     ▲▲▲ ▲                  ▲▲ ▲▲  ▲   ▲▲

.4byte 0xfe1009db # cgettype x19, x0
#                                   ▼     ▼       ▼▼         ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                                                           ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                 ▼▼ ▼                   ▼▼▼▼   ▼   ▼
#      Trap: False, PCWD: 0x0000000080000010, RD: 19, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe1009db PRV_? XL:? (cgettype x19, x0)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfe1009db != 0xfe7880db, mismatch in field rd_addr: 19 != 0, mismatch in field rd_wdata: 0xffffffffffffffff != 0x0, mismatch in field rs2_addr: 1 != 0, mismatch in field pc_wdata: 0x80000010 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                         ▲▲      ▲▲         ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                                                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                 ▲▲▲                    ▲▲▲▲▲      ▲▲

.4byte 0xfe62025b # cgetoffset x4, x4
#                                   ▼     ▼▼       ▼                                                                                                                                                     ▼▼ ▼▼                  ▼ ▼ ▼▼  ▼   ▼
#      Trap: False, PCWD: 0x0000000080000014, RD: 04, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe62025b PRV_? XL:? (cgetoffset x4, x4)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfe62025b != 0xfe7880db, mismatch in field rd_addr: 4 != 0, mismatch in field rs1_addr: 4 != 0, mismatch in field rs2_addr: 6 != 0, mismatch in field pc_wdata: 0x80000014 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                        ▲▲▲       ▲                                                                                                                                                     ▲▲▲ ▲                   ▲▲▲   ▲   ▲▲

.4byte 0xfe70815b # cgetflags x2, x1
#                                   ▼     ▼▼       ▼                                                                                                                                                      ▼ ▼▼                         ▼
#      Trap: False, PCWD: 0x0000000080000018, RD: 02, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe70815b PRV_? XL:? (cgetflags x2, x1)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfe70815b != 0xfe7880db, mismatch in field rd_addr: 2 != 0, mismatch in field rs1_addr: 1 != 0, mismatch in field rs2_addr: 7 != 0, mismatch in field pc_wdata: 0x80000018 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                        ▲▲▲       ▲                                                                                                                                                       ▲▲▲                         ▲    ▲

.4byte 0xfe1900db # cgettype x1, x18
#                                   ▼     ▼▼       ▼         ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                                                           ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                 ▼▼▼                    ▼▼▼▼       ▼
#      Trap: False, PCWD: 0x000000008000001c, RD: 01, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe1900db PRV_? XL:? (cgettype x1, x18)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfe1900db != 0xfe7880db, mismatch in field rd_addr: 1 != 0, mismatch in field rd_wdata: 0xffffffffffffffff != 0x0, mismatch in field rs1_addr: 18 != 0, mismatch in field rs2_addr: 1 != 0, mismatch in field pc_wdata: 0x8000001c != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                        ▲▲▲       ▲         ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                                                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                 ▲▲▲                    ▲▲▲▲▲       ▲

.4byte 0xff79095b # cgethigh x18, x18
#            ▼▼▼▼                                 ▼▼                                                                                                                                                    ▼ ▼ ▼▼                  ▼▼ ▼   ▼    ▼
#      Trap:  True, PCWD: 0x0000000000000000, RD: 18, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000ff79095b PRV_? XL:? (cgethigh x18, x18)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xff79095b != 0xfe7880db, mismatch in field trap: 1 != 0, mismatch in field rd_addr: 18 != 0, mismatch in field rs1_addr: 18 != 0, mismatch in field rs2_addr: 23 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#            ▲▲▲▲                                 ▲▲                                                                                                                                                    ▲ ▲▲ ▲                  ▲▲▲ ▲       ▲

.4byte 0xfe30085b # cgetlen x16, x0
#                                          ▼      ▼▼         ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                                                           ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                 ▼▼▼ ▼                   ▼▼   ▼   ▼
#      Trap: False, PCWD: 0x0000000000000004, RD: 16, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe30085b PRV_? XL:? (cgetlen x16, x0)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfe30085b != 0xfe7880db, mismatch in field rd_addr: 16 != 0, mismatch in field rd_wdata: 0xffffffffffffffff != 0x0, mismatch in field rs2_addr: 3 != 0, mismatch in field pc_wdata: 0x4 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                          ▲      ▲▲         ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                                                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                 ▲ ▲▲▲                  ▲ ▲▲▲      ▲▲

.4byte 0xfe79025b # cgetflags x4, x18
#                                          ▼       ▼                                                                                                                                                      ▼ ▼▼                         ▼    ▼
#      Trap: False, PCWD: 0x0000000000000008, RD: 04, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe79025b PRV_? XL:? (cgetflags x4, x18)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfe79025b != 0xfe7880db, mismatch in field rd_addr: 4 != 0, mismatch in field rs1_addr: 18 != 0, mismatch in field rs2_addr: 7 != 0, mismatch in field pc_wdata: 0x8 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                          ▲       ▲                                                                                                                                                      ▲▲ ▲                         ▲    ▲

.4byte 0xac619903 # lh x18, x3[-1338]
#            ▼▼▼▼                                            ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼        ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                                   ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼               ▼▼              ▼▼▼▼▼▼ ▼              ▼   ▼   ▼▼▼ ▼▼▼▼
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0xaaaaaaaaaaaaaaaa, MA: 0xfffffffffffffac6, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xaaaaaaaaaaaaaaaa, MRM: 0b00000011 I: 0x00000000ac619903 PRV_? XL:? (lh x18, x3[-1338])
# ╷
# │ ^ A, B v: mismatch in field insn: 0xac619903 != 0xfe7880db, mismatch in field trap: 1 != 0, mismatch in field rd_wdata: 0xaaaaaaaaaaaaaaaa != 0x0, mismatch in field rs1_addr: 3 != 0, mismatch in field rs2_addr: 6 != 0, mismatch in field mem_addr: 0xfffffffffffffac6 != 0x0, mismatch in field mem_rdata: 
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#            ▲▲▲▲                                            ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲        ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                                   ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲               ▲▲              ▲▲▲▲▲ ▲▲             ▲▲▲▲▲ ▲▲▲       ▲

.4byte 0x00100073 # ebreak
#            ▼▼▼▼                                                                                                                                                                                      ▼▼▼▼▼▼ ▼             ▼▼▼  ▼
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x0000000000100073 PRV_? XL:? (ebreak)
# ╷
# │ ^ A, B v: mismatch in field insn: 0x100073 != 0xfe7880db, mismatch in field trap: 1 != 0, mismatch in field rs2_addr: 1 != 0, mismatch in field rs2_rdata: 0xffffffffffffffff != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#            ▲▲▲▲                                                                                                                                                                                      ▲▲ ▲▲▲▲▲             ▲▲ ▲▲▲ ▲▲▲▲▲▲▲▲▲▲

.4byte 0xfe8089db # croundrepresentablelength x19, x1
#                                          ▼      ▼▼                                                                                                                                                      ▼ ▼                ▼▼▼▼▼▼▼▼▼▼▼ ▼ ▼▼▼▼ ▼▼ ▼▼   ▼
#      Trap: False, PCWD: 0x0000000000000004, RD: 19, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe8089db PRV_? XL:? (croundrepresentablelength x19, x1)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfe8089db != 0xfe7880db, mismatch in field rd_addr: 19 != 0, mismatch in field rs1_addr: 1 != 0, mismatch in field rs1_rdata: 0xffffffffffffffff != 0x0, mismatch in field rs2_addr: 8 != 0, mismatch in field pc_wdata: 0x4 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                          ▲      ▲▲                                                                                                                                                     ▲  ▲                ▲  ▲ ▲ ▲       ▲

.4byte 0x2c184003 # lbu x0, x16[705]
#            ▼▼▼▼                                                                                 ▼▼          ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                          ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                ▼              ▼▼▼ ▼▼ ▼              ▼▼  ▼    ▼▼ ▼▼▼
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x00000000000002c0, MWD: 0xffffffffffffffff, MWM: 0b00000000, MRD: 0xaaaaaaaaaaaaaaaa, MRM: 0b00000001 I: 0x000000002c184003 PRV_? XL:? (lbu x0, x16[705])
# ╷
# │ ^ A, B v: mismatch in field insn: 0x2c184003 != 0xfe7880db, mismatch in field trap: 1 != 0, mismatch in field rs1_addr: 16 != 0, mismatch in field rs1_rdata: 0xffffffffffffffff != 0x0, mismatch in field rs2_addr: 1 != 0, mismatch in field rs2_rdata: 0xffffffffffffffff != 0x0, mismatch in field mem_addr: 0x2c0 != 0x0, mismatch in field mem_rdata: 
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#            ▲▲▲▲                                                                                  ▲▲         ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                          ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                ▲              ▲▲▲ ▲ ▲▲             ▲▲▲▲▲ ▲▲▲  ▲

.4byte 0xfe82025b # croundrepresentablelength x4, x4
#                                          ▼       ▼                                                                                                                                                      ▼ ▼▼               ▼▼▼▼▼▼▼▼▼▼▼ ▼ ▼▼▼▼ ▼▼ ▼▼  ▼   ▼
#      Trap: False, PCWD: 0x0000000000000004, RD: 04, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe82025b PRV_? XL:? (croundrepresentablelength x4, x4)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfe82025b != 0xfe7880db, mismatch in field rd_addr: 4 != 0, mismatch in field rs1_addr: 4 != 0, mismatch in field rs2_addr: 8 != 0, mismatch in field pc_wdata: 0x4 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                          ▲       ▲                                                                                                                                                     ▲ ▲ ▲               ▲  ▲ ▲ ▲  ▲   ▲▲

.4byte 0x01227a33 # and x20, x4, x18
#                                          ▼      ▼                                                                                                                                                    ▼▼▼▼ ▼▼▼              ▼▼▼▼▼▼▼  ▼    ▼
#      Trap: False, PCWD: 0x0000000000000008, RD: 20, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x0000000001227a33 PRV_? XL:? (and x20, x4, x18)
# ╷
# │ ^ A, B v: mismatch in field insn: 0x1227a33 != 0xfe7880db, mismatch in field rd_addr: 20 != 0, mismatch in field rs1_addr: 4 != 0, mismatch in field rs2_addr: 18 != 0, mismatch in field pc_wdata: 0x8 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                          ▲       ▲                                                                                                                                                   ▲▲ ▲▲▲▲▲             ▲▲▲▲▲▲ ▲▲  ▲    ▲

.4byte 0xfe9989db # crepresentablealignmentmask x19, x19
#                                          ▼      ▼▼         ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                                                           ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                 ▼▼ ▼                ▼▼▼▼▼▼ ▼ ▼▼ ▼ ▼▼ ▼▼▼▼▼▼▼ ▼   ▼    ▼
#      Trap: False, PCWD: 0x000000000000000c, RD: 19, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe9989db PRV_? XL:? (crepresentablealignmentmask x19, x19)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfe9989db != 0xfe7880db, mismatch in field rd_addr: 19 != 0, mismatch in field rd_wdata: 0xffffffffffffffff != 0x0, mismatch in field rs1_addr: 19 != 0, mismatch in field rs2_addr: 9 != 0, mismatch in field pc_wdata: 0xc != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                          ▲      ▲▲         ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                                                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                 ▲ ▲▲                ▲  ▲           ▲

.4byte 0xfe30025b # cgetlen x4, x0
#                                         ▼        ▼         ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                                                           ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                 ▼▼ ▼▼                   ▼▼  ▼   ▼
#      Trap: False, PCWD: 0x0000000000000010, RD: 04, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe30025b PRV_? XL:? (cgetlen x4, x0)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfe30025b != 0xfe7880db, mismatch in field rd_addr: 4 != 0, mismatch in field rd_wdata: 0xffffffffffffffff != 0x0, mismatch in field rs2_addr: 3 != 0, mismatch in field pc_wdata: 0x10 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                          ▲       ▲         ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                                                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                 ▲▲▲ ▲                  ▲ ▲▲▲  ▲   ▲▲

.4byte 0xfe48085b # cgettag x16, x16
#                                         ▼▼      ▼▼                                                          ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                                                           ▼ ▼ ▼                  ▼     ▼    ▼
#      Trap: False, PCWD: 0x0000000000000014, RD: 16, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0xffffffffffffffff, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe48085b PRV_? XL:? (cgettag x16, x16)
# ╷
# │ ^ A, B v: mismatch in field insn: 0xfe48085b != 0xfe7880db, mismatch in field rd_addr: 16 != 0, mismatch in field rs1_addr: 16 != 0, mismatch in field rs1_rdata: 0xffffffffffffffff != 0x0, mismatch in field rs2_addr: 4 != 0, mismatch in field rs2_rdata: 0xffffffffffffffff != 0x0, mismatch in field pc_wdata: 0x14 != 0x0
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#                                         ▲▲      ▲▲                                                          ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                                                           ▲  ▲▲                  ▲▲  ▲       ▲

.4byte 0x1738a083 # lw x1, x17[371]
#            ▼▼▼▼                                            ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                     ▼▼▼         ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                          ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼             ▼▼▼▼              ▼ ▼ ▼▼ ▼              ▼        ▼▼▼▼▼
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0xaaaaaaaaaaaaaaaa, MA: 0x0000000000000173, MWD: 0xffffffffffffffff, MWM: 0b00000000, MRD: 0xaaaaaaaaaaaaaaaa, MRM: 0b00001111 I: 0x000000001738a083 PRV_? XL:? (lw x1, x17[371])
# ╷
# │ ^ A, B v: mismatch in field insn: 0x1738a083 != 0xfe7880db, mismatch in field trap: 1 != 0, mismatch in field rd_wdata: 0xaaaaaaaaaaaaaaaa != 0x0, mismatch in field rs1_addr: 17 != 0, mismatch in field rs2_addr: 19 != 0, mismatch in field rs2_rdata: 0xffffffffffffffff != 0x0, mismatch in field mem_addr: 0x173 != 0x0, mismatch in field mem_rdata: 
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#            ▲▲▲▲                                            ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                     ▲▲▲         ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                          ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲             ▲▲▲▲              ▲▲   ▲▲▲             ▲▲▲▲▲ ▲▲▲

# Test end
#      ▼  ▼ ▼▼▼ ▼
#      halt token
# ╷
# │ ^ A, B v: expected halt package
# ╵
#      Trap: False, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7880db PRV_? XL:? (cgetflags x1, x17)
#      ▲▲ ▲▲▲▲▲ ▲▲▲ ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲ ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲
