# 
#>QCVENGINE_TEST_V2.0
.4byte 0xfe88085b # croundrepresentablelength x16, x16
#      Trap: False, PCWD: 0x0000000080000004, RD: 16, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe88085b PRV_? XL:? (croundrepresentablelength x16, x16)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 16 != 0, mismatch in field rs2_addr: 8 != 0
# ╵
#      Trap: False, PCWD: 0x0000000080000004, RD: 16, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe88085b PRV_? XL:? (croundrepresentablelength x16, x16)

.4byte 0x8658c103 # lbu x2, x17[-1947]
#                                                            ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                                                           ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                ▼
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0xaaaaaaaaaaaaaaaa, MA: 0xfffffffffffff865, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xaaaaaaaaaaaaaaaa, MRM: 0b00000001 I: 0x000000008658c103 PRV_? XL:? (lbu x2, x17[-1947])
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0xaaaaaaaaaaaaaaaa != 0x0, mismatch in field rs1_addr: 17 != 0, mismatch in field rs2_addr: 5 != 0, mismatch in field mem_rdata: 
# ╵
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0xfffffffffffff865, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x000000008658c103 PRV_? XL:? (lbu x2, x17[-1947])
#                                                            ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                                                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                ▲

.4byte 0xfe9180db # crepresentablealignmentmask x1, x3
#                                                                                                                                                       ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000000000004, RD: 01, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe9180db PRV_? XL:? (crepresentablealignmentmask x1, x3)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 3 != 0, mismatch in field rs2_addr: 9 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000004, RD: 01, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe9180db PRV_? XL:? (crepresentablealignmentmask x1, x3)
#                                                                                                                                                       ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xfe7988db # cgetflags x17, x19
#      Trap: False, PCWD: 0x0000000000000008, RD: 17, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7988db PRV_? XL:? (cgetflags x17, x19)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 19 != 0, mismatch in field rs2_addr: 7 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000008, RD: 17, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7988db PRV_? XL:? (cgetflags x17, x19)

.4byte 0xfef008db # cgetaddr x17, x0
#      Trap: False, PCWD: 0x000000000000000c, RD: 17, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fef008db PRV_? XL:? (cgetaddr x17, x0)
# ╷
# │ ^ A, B v: mismatch in field rs2_addr: 15 != 0
# ╵
#      Trap: False, PCWD: 0x000000000000000c, RD: 17, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fef008db PRV_? XL:? (cgetaddr x17, x0)

.4byte 0x05b8a217 # auipc x4, 23434
#                                                                                             ▼▼▼▼▼                                                              ▼▼▼▼  ▼
#      Trap: False, PCWD: 0x0000000000000010, RD: 04, RWD: 0x0000000005b8a00c, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000005b8a00c, MRM: 0b00000000 I: 0x0000000005b8a217 PRV_? XL:? (auipc x4, 23434)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 17 != 0, mismatch in field rs2_addr: 27 != 0, mismatch in field mem_addr: 0x0 != 0x5b8a00c
# ╵
#      Trap: False, PCWD: 0x0000000000000010, RD: 04, RWD: 0x0000000005b8a00c, MA: 0x0000000005b8a00c, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x0000000005b8a217 PRV_? XL:? (auipc x4, 23434)
#                                                                                             ▲▲▲▲  ▲                                                              ▲▲▲▲▲

.4byte 0x014808b3 # add x17, x16, x20
#      Trap: False, PCWD: 0x0000000000000014, RD: 17, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000014808b3 PRV_? XL:? (add x17, x16, x20)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 16 != 0, mismatch in field rs2_addr: 20 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000014, RD: 17, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000014808b3 PRV_? XL:? (add x17, x16, x20)

.4byte 0xfef9c083 # lbu x1, x19[-17]
#                                                            ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                                                                           ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                ▼
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0xaaaaaaaaaaaaaaaa, MA: 0xffffffffffffffef, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xaaaaaaaaaaaaaaaa, MRM: 0b00000001 I: 0x00000000fef9c083 PRV_? XL:? (lbu x1, x19[-17])
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0xaaaaaaaaaaaaaaaa != 0x0, mismatch in field rs1_addr: 19 != 0, mismatch in field rs2_addr: 15 != 0, mismatch in field mem_rdata: 
# ╵
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0xffffffffffffffef, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fef9c083 PRV_? XL:? (lbu x1, x19[-17])
#                                                            ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                                                                           ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                ▲

.4byte 0xfe7a085b # cgetflags x16, x20
#      Trap: False, PCWD: 0x0000000000000004, RD: 16, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7a085b PRV_? XL:? (cgetflags x16, x20)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 20 != 0, mismatch in field rs2_addr: 7 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000004, RD: 16, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe7a085b PRV_? XL:? (cgetflags x16, x20)

.4byte 0xfe8188db # croundrepresentablelength x17, x3
#      Trap: False, PCWD: 0x0000000000000008, RD: 17, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe8188db PRV_? XL:? (croundrepresentablelength x17, x3)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 3 != 0, mismatch in field rs2_addr: 8 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000008, RD: 17, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe8188db PRV_? XL:? (croundrepresentablelength x17, x3)

.4byte 0xfe82005b # croundrepresentablelength x0, x4
#                                                                                             ▼▼▼▼▼                                                              ▼▼▼
#      Trap: False, PCWD: 0x000000000000000c, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000005ba0000, MRM: 0b00000000 I: 0x00000000fe82005b PRV_? XL:? (croundrepresentablelength x0, x4)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 4 != 0, mismatch in field rs1_rdata: 0x5b8a00c != 0x0, mismatch in field rs2_addr: 8 != 0, mismatch in field mem_addr: 0x0 != 0x5b8a00c
# ╵
#      Trap: False, PCWD: 0x000000000000000c, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000005b8a00c, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe82005b PRV_? XL:? (croundrepresentablelength x0, x4)
#                                                                                             ▲▲▲▲  ▲                                                                ▲▲▲

.4byte 0x00000073 # ecall
#      Trap:  True, PCWD: 0x0000000000000000, RD: 00, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x0000000000000073 PRV_? XL:? (ecall)

.4byte 0xfe51895b # cgetsealed x18, x3
#      Trap: False, PCWD: 0x0000000000000004, RD: 18, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe51895b PRV_? XL:? (cgetsealed x18, x3)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 3 != 0, mismatch in field rs2_addr: 5 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000004, RD: 18, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe51895b PRV_? XL:? (cgetsealed x18, x3)

.4byte 0xfe0a085b # cgetperm x16, x20
#                                                                       ▼▼▼▼▼                                                                                      ▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000000000008, RD: 16, RWD: 0x0000000000078fff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000078fff, MRM: 0b00000000 I: 0x00000000fe0a085b PRV_? XL:? (cgetperm x16, x20)
# ╷
# │ ^ A, B v: mismatch in field rd_wdata: 0x78fff != 0x0, mismatch in field rs1_addr: 20 != 0
# ╵
#      Trap: False, PCWD: 0x0000000000000008, RD: 16, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe0a085b PRV_? XL:? (cgetperm x16, x20)
#                                                                       ▲▲▲▲▲                                                                                      ▲▲▲▲▲

.4byte 0x01425833 # srl x16, x4, x20
#                                                                                             ▼▼▼▼▼                                                              ▼▼▼▼  ▼
#      Trap: False, PCWD: 0x000000000000000c, RD: 16, RWD: 0x0000000005b8a00c, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000005b8a00c, MRM: 0b00000000 I: 0x0000000001425833 PRV_? XL:? (srl x16, x4, x20)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 4 != 0, mismatch in field rs1_rdata: 0x5b8a00c != 0x0, mismatch in field rs2_addr: 20 != 0, mismatch in field mem_addr: 0x0 != 0x5b8a00c
# ╵
#      Trap: False, PCWD: 0x000000000000000c, RD: 16, RWD: 0x0000000005b8a00c, MA: 0x0000000005b8a00c, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x0000000001425833 PRV_? XL:? (srl x16, x4, x20)
#                                                                                             ▲▲▲▲  ▲                                                              ▲▲▲▲▲

.4byte 0xbfd20a67 # jalr x20, x4, -1027
#                                                                                             ▼▼▼▼▼▼                                                                  ▼
#      Trap: False, PCWD: 0x0000000005b89c08, RD: 20, RWD: 0x0000000000000010, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000010, MRM: 0b00000000 I: 0x00000000bfd20a67 PRV_? XL:? (jalr x20, x4, -1027)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 4 != 0, mismatch in field rs1_rdata: 0x5b8a00c != 0x0, mismatch in field rs2_addr: 29 != 0, mismatch in field mem_addr: 0x0 != 0x5b89c09
# ╵
#      Trap: False, PCWD: 0x0000000005b89c08, RD: 20, RWD: 0x0000000000000010, MA: 0x0000000005b89c09, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000bfd20a67 PRV_? XL:? (jalr x20, x4, -1027)
#                                                                                             ▲▲▲▲▲ ▲                                                                  ▲

.4byte 0xfe3a0a5b # cgetlen x20, x20
#                                                                                                  ▼                                                    ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
#      Trap: False, PCWD: 0x0000000005b89c0c, RD: 20, RWD: 0xffffffffffffffff, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0xffffffffffffffff, MRM: 0b00000000 I: 0x00000000fe3a0a5b PRV_? XL:? (cgetlen x20, x20)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 20 != 0, mismatch in field rs1_rdata: 0x10 != 0x0, mismatch in field rs2_addr: 3 != 0, mismatch in field mem_addr: 0x0 != 0x10
# ╵
#      Trap: False, PCWD: 0x0000000005b89c0c, RD: 20, RWD: 0xffffffffffffffff, MA: 0x0000000000000010, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe3a0a5b PRV_? XL:? (cgetlen x20, x20)
#                                                                                                  ▲                                                    ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲

.4byte 0xfef180db # cgetaddr x1, x3
#      Trap: False, PCWD: 0x0000000005b89c10, RD: 01, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fef180db PRV_? XL:? (cgetaddr x1, x3)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 3 != 0, mismatch in field rs2_addr: 15 != 0
# ╵
#      Trap: False, PCWD: 0x0000000005b89c10, RD: 01, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fef180db PRV_? XL:? (cgetaddr x1, x3)

.4byte 0x01285933 # srl x18, x16, x18
#                                                                                             ▼▼▼▼▼                                                              ▼▼▼▼  ▼
#      Trap: False, PCWD: 0x0000000005b89c14, RD: 18, RWD: 0x0000000005b8a00c, MA: 0x0000000000000000, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000005b8a00c, MRM: 0b00000000 I: 0x0000000001285933 PRV_? XL:? (srl x18, x16, x18)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 16 != 0, mismatch in field rs1_rdata: 0x5b8a00c != 0x0, mismatch in field rs2_addr: 18 != 0, mismatch in field mem_addr: 0x0 != 0x5b8a00c
# ╵
#      Trap: False, PCWD: 0x0000000005b89c14, RD: 18, RWD: 0x0000000005b8a00c, MA: 0x0000000005b8a00c, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x0000000001285933 PRV_? XL:? (srl x18, x16, x18)
#                                                                                             ▲▲▲▲  ▲                                                              ▲▲▲▲▲

.4byte 0xfe4a015b # cgettag x2, x20
#                                                                                    ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                  ▼▼▼▼  ▼
#      Trap: False, PCWD: 0x0000000005b89c18, RD: 02, RWD: 0x0000000000000000, MA: 0x0000000000000000, MWD: 0x0000000005b8a00c, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe4a015b PRV_? XL:? (cgettag x2, x20)
# ╷
# │ ^ A, B v: mismatch in field rs1_addr: 20 != 0, mismatch in field rs1_rdata: 0xffffffffffffffff != 0x0, mismatch in field rs2_addr: 4 != 0, mismatch in field rs2_rdata: 0x5b8a00c != 0x0, mismatch in field mem_addr: 0x0 != 0xffffffffffffffff
# ╵
#      Trap: False, PCWD: 0x0000000005b89c18, RD: 02, RWD: 0x0000000000000000, MA: 0xffffffffffffffff, MWD: 0x0000000000000000, MWM: 0b00000000, MRD: 0x0000000000000000, MRM: 0b00000000 I: 0x00000000fe4a015b PRV_? XL:? (cgettag x2, x20)
#                                                                                    ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲                    ▲▲▲▲▲

# Test end
#      halt token
