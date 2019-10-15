--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018 Hesham Almatary
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2019 Alexandre Joannou
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory (Department of Computer Science and
-- Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
-- DARPA SSITH research programme.
--
-- This software was partly developed by the University of Cambridge
-- Computer Laboratory as part of the Partially-Ordered Event-Triggered
-- Systems (POETS) project, funded by EPSRC grant EP/N031768/1.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
--

module RISCV.RV32_Xcheri (
  rv32_xcheri_disass
, rv32_xcheri
, rv32_xcheri_inspection
, rv32_xcheri_arithmetic
, rv32_xcheri_misc
, rv32_xcheri_mem
, rv32_xcheri_control
, cgetperm
, cgettype
, cgetbase
, cgetlen
, cgettag
, cgetsealed
, cgetoffset
, cgetflags
, cgetaddr
, cseal
, cunseal
, candperm
, csetflags
, csetoffset
, csetaddr
, cincoffset
, cincoffsetimmediate
, csetbounds
, csetboundsexact
, csetboundsimmediate
, ccleartag
, cbuildcap
, ccopytype
, ccseal
, ctoptr
, cfromptr
, cmove
, cspecialrw
, cjalr
, ccall
, ctestsubset
, clear
, fpclear
, cload
, cstore
, lq
, sq
) where

import RISCV.Helpers (reg, int, prettyR, prettyI, prettyL, prettyS, prettyR_2op)
import InstrCodec (DecodeBranch, (-->), encode)

---------------------
-- CHERI instructions
---------------------

-- Capability Inspection
cgetperm                  = "1111111 00000 cs1[4:0] 000 rd[4:0] 1011011"
cgettype                  = "1111111 00001 cs1[4:0] 000 rd[4:0] 1011011"
cgetbase                  = "1111111 00010 cs1[4:0] 000 rd[4:0] 1011011"
cgetlen                   = "1111111 00011 cs1[4:0] 000 rd[4:0] 1011011"
cgettag                   = "1111111 00100 cs1[4:0] 000 rd[4:0] 1011011"
cgetsealed                = "1111111 00101 cs1[4:0] 000 rd[4:0] 1011011"
cgetoffset                = "1111111 00110 cs1[4:0] 000 rd[4:0] 1011011"
cgetflags                 = "1111111 00111 cs1[4:0] 000 rd[4:0] 1011011"
cgetaddr                  = "1111111 01111 cs1[4:0] 000 rd[4:0] 1011011"

-- Capability Modification
cseal                     = "0001011 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cunseal                   = "0001100 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
candperm                  = "0001101 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetflags                 = "0001110 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetoffset                = "0001111 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetaddr                  = "0010000 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cincoffset                = "0010001 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cincoffsetimmediate       = "imm[11:0] cs1[4:0] 001 cd[4:0] 1011011"
csetbounds                = "0001000 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetboundsexact           = "0001001 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetboundsimmediate       = "imm[11:0] cs1[4:0] 010 cd[4:0] 1011011"
ccleartag                 = "1111111 01011 cs1[4:0] 000 cd[4:0] 1011011"
cbuildcap                 = "0011101 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
ccopytype                 = "0011110 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
ccseal                    = "0011111 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"

-- Capability Pointer Arithmetic
ctoptr                    = "0010010 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cfromptr                  = "0010011 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cmove                     = "1111111 01010 cs1[4:0] 000 cd[4:0] 1011011"
cspecialrw                = "0000001 cSP[4:0] cs1[4:0] 000 cd[4:0] 1011011"

-- Control Flow
cjalr                     = "1111111 01100 cs1[4:0] 000 cd[4:0] 1011011"
ccall                     = "1111110 pcc[4:0] idc[4:0] 000 00001 1011011"

-- Assertion
ctestsubset               = "0100000 cs2[4:0] cs1[4:0] 000 rd[4:0] 1011011"

-- Register Clearing
clear                     = "1111111 01101 q[1:0] imm[7:5] 000 imm[4:0] 1011011"
fpclear                   = "1111111 10000 q[1:0] imm[7:5] 000 imm[4:0] 1011011"

-- Memory -- Needs further refinement
cload                      = "1111101 mop[4:0] cb[4:0] 000 cd[4:0] 1011011"
cstore                     = "1111100 rs1[4:0] rs2[4:0] 000 mop[4:0] 1011011"
lq                         = "imm[11:0] rs1[4:0] 010 cd[4:0] 0001111"
sq                         = "imm[11:5] cs2[4:0] rs1[4:0] 100 imm[4:0] 0100011"

-----------------------------
-- Instruction pretty printer
-----------------------------

prettyCLoad :: Integer -> Integer -> Integer -> String
prettyCLoad mop rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, "[0]"]
    where instr = case mop of 0x00 -> "LBddc"
                              0x01 -> "LHddc"
                              0x02 -> "LWddc"
                              0x03 -> "LDddc"
                              0x04 -> "LBUddc"
                              0x05 -> "LHUddc"
                              0x06 -> "LWUddc"
                              0x07 -> "LDUddc"  -- TODO clarify meaning...
                              0x08 -> "LBcap"
                              0x09 -> "LHcap"
                              0x0a -> "LWcap"
                              0x0b -> "LDcap"
                              0x0c -> "LBUcap"
                              0x0d -> "LHUcap"
                              0x0e -> "LWUcap"
                              0x0f -> "LDUcap"  -- TODO clarify meaning...
                              0x10 -> "LRddc.B"
                              0x11 -> "LRddc.H"
                              0x12 -> "LRddc.W"
                              0x13 -> "LRddc.D"
                              0x14 -> "LRddc.Q" -- TODO only valid in rv64
                              0x15 -> "INVALID"
                              0x16 -> "INVALID"
                              0x17 -> "LQddc"   -- TODO only valid in rv64
                              0x18 -> "LRcap.B"
                              0x19 -> "LRcap.H"
                              0x1a -> "LRcap.W"
                              0x1b -> "LRcap.D"
                              0x1c -> "LRcap.Q" -- TODO only valid in rv64
                              0x1d -> "INVALID"
                              0x1e -> "INVALID"
                              0x1f -> "LQcap"   -- TODO only valid in rv64
                              _    -> "INVALID"

prettyCStore :: Integer -> Integer -> Integer -> String
prettyCStore rs2 rs1 mop =
  concat [instr, " ", reg rs2, ", ", reg rs1, "[0]"]
    where instr = case mop of 0x00 -> "SBddc"
                              0x01 -> "SHddc"
                              0x02 -> "SWddc"
                              0x03 -> "SDddc"
                              0x04 -> "SQddc"   -- TODO only valid in rv64
                              0x08 -> "SBcap"
                              0x09 -> "SHcap"
                              0x0a -> "SWcap"
                              0x0b -> "SDcap"
                              0x0c -> "SQcap"   -- TODO only valid in rv64
                              0x10 -> "SCddc.B"
                              0x11 -> "SCddc.H"
                              0x12 -> "SCddc.W"
                              0x13 -> "SCddc.D"
                              0x14 -> "SCddc.Q" -- TODO only valid in rv64
                              0x18 -> "SCcap.B"
                              0x19 -> "SCcap.H"
                              0x1a -> "SCcap.W"
                              0x1b -> "SCcap.D"
                              0x1c -> "SCcap.Q" -- TODO only valid in rv64
                              _ -> "INVALID"

-- R-type, 2-operand pretty printer
pretty_reg_clear instr imm qt =
  concat [instr, " ", int qt, ", ", int imm]

pretty_2src instr idc pcc =
  concat [instr, " ", reg pcc, ", ", reg idc]

pretty_cspecialrw instr idx cs1 cd =
  concat [instr, " ", reg cd, ", ", reg cs1, ", ", int idx]

rv32_xcheri_disass :: [DecodeBranch String]
rv32_xcheri_disass = [
     cgetperm            --> prettyR_2op "cgetperm"
   , cgettype            --> prettyR_2op "cgettype"
   , cgetbase            --> prettyR_2op "cgetbase"
   , cgetlen             --> prettyR_2op "cgetlen"
   , cgettag             --> prettyR_2op "cgettag"
   , cgetsealed          --> prettyR_2op "cgetsealed"
   , cgetoffset          --> prettyR_2op "cgetoffset"
   , cgetaddr            --> prettyR_2op "cgetaddr"
   , cseal               --> prettyR "cseal"
   , cunseal             --> prettyR "cunseal"
   , candperm            --> prettyR "candperm"
   , csetoffset          --> prettyR "csetoffset"
   , csetaddr            --> prettyR "csetaddr"
   , cincoffset          --> prettyR "cincoffset"
   , csetbounds          --> prettyR "csetbounds"
   , csetboundsexact     --> prettyR "csetboundsexact"
   , cbuildcap           --> prettyR "cbuildcap"
   , ccopytype           --> prettyR "ccopytype"
   , ccseal              --> prettyR "ccseal"
   , ccleartag           --> prettyR_2op "ccleartag"
   , cincoffsetimmediate --> prettyI "cincoffsetimmediate"
   , csetboundsimmediate --> prettyI "csetboundsimmediate"
   , ctoptr              --> prettyR "ctoptr"
   , cfromptr            --> prettyR "cfromptr"
   , cspecialrw          --> pretty_cspecialrw "cspecialrw"
   , cmove               --> prettyR_2op "cmove"
   , cjalr               --> prettyR_2op "cjalr"
   , ccall               --> pretty_2src "ccall"
   , ctestsubset         --> prettyR "ctestsubset"
   , clear               --> pretty_reg_clear "clear"
   , fpclear             --> pretty_reg_clear "fpclear"
   , cload               --> prettyCLoad
   , cstore              --> prettyCStore
   , cgetflags           --> prettyR_2op "cgetflags"
   , csetflags           --> prettyR "csetflags"
   , sq                  --> prettyS "sq"
   , lq                  --> prettyL "lq"
  ]

rv32_xcheri_inspection :: Integer -> Integer -> [Integer]
rv32_xcheri_inspection src dest = [
    encode cgetperm src dest
 ,  encode cgettype src dest
 ,  encode cgetbase src dest
 ,  encode cgetlen src dest
 ,  encode cgettag src dest
 ,  encode cgetsealed src dest
 ,  encode cgetoffset src dest
 ,  encode cgetaddr src dest
 ,  encode cgetflags src dest
  ]

rv32_xcheri_arithmetic :: Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_xcheri_arithmetic src1 src2 imm dest = [
    encode csetoffset src1 src2 dest
 ,  encode csetaddr   src1 src2 dest
 ,  encode cincoffset src1 src2 dest
 ,  encode csetbounds src1 src2 dest
 ,  encode csetboundsexact src1 src2 dest
 ,  encode csetboundsimmediate imm src1 dest
 ,  encode cincoffsetimmediate imm src1 dest
 ,  encode ctoptr     src1 src2 dest
 ,  encode cfromptr   src1 src2 dest
 ,  encode ctestsubset src1 src2 dest
  ]

rv32_xcheri_misc :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_xcheri_misc src1 src2 srcScr imm dest = [
    encode cseal      src1 src2 dest
 ,  encode cunseal    src1 src2 dest
 ,  encode candperm   src1 src2 dest
 ,  encode cbuildcap  src1 src2 dest
 ,  encode csetflags  src1 src2 dest
 ,  encode ccopytype  src1 src2 dest
 ,  encode ccseal     src1 src2 dest
 ,  encode ccleartag  src1 dest
 ,  encode cspecialrw srcScr src1 dest
  ]

rv32_xcheri_control :: Integer -> Integer -> Integer -> [Integer]
rv32_xcheri_control src1 src2 dest = [
    encode cjalr src1 dest
 ,  encode ccall src1 src2
  ]

rv32_xcheri_mem :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_xcheri_mem srcAddr srcData imm mop dest = [
    encode cload mop srcAddr dest
 ,  encode cstore srcData srcAddr mop
-- ,  encode ld imm srcAddr dest
-- ,  encode sd imm srcAddr srcAddr
-- ,  encode lq imm srcAddr dest
-- ,  encode sq imm srcData srcAddr
  ]

rv32_xcheri :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_xcheri src1 src2 srcScr imm mop dest =
     rv32_xcheri_inspection src1 dest
  ++ rv32_xcheri_arithmetic src1 src2 imm dest
  ++ rv32_xcheri_misc src1 src2 srcScr imm dest
  ++ rv32_xcheri_control src1 src2 dest
  ++ rv32_xcheri_mem src1 src2 imm mop dest
