--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Hesham Almatary 
-- Copyright (c) 2018 Matthew Naylor
-- All rights reserved.
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

module CHERI where

import InstrCodec
import Test.QuickCheck
import Control.Monad
import RISCV

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
cgetaddr                  = "1111111 01111 cs1[4:0] 000 rd[4:0] 1011011"

-- Capability Modification
cseal                     = "0001011 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cunseal                   = "0001100 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
candperm                  = "0001101 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cbuildcap                 = "0011101 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
ccopytype                 = "0011110 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
ccseal                    = "0011111 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
ccleartag                 = "1111111 01011 cs1[4:0] 000 cd[4:0] 1011011"

-- Capability Pointer Arithmetic
csetoffset                = "0001111 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cincoffset                = "0010001 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetbounds                = "0001000 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetboundsexact           = "0001001 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cincoffsetimmediate       = "imm[11:0] cs1[4:0] 001 cd[4:0] 1011011"
csetboundsimmediate       = "imm[11:0] cs1[4:0] 002 cd[4:0] 1011011"
ctoptr                    = "0010010 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cfromptr                  = "0010011 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cspecialrw                = "0000001 cSP[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cmove                     = "1111111 01010 cs1[4:0] 000 cd[4:0] 1011011"

-- Control Flow
cjalr                     = "1111111 01100 cs1[4:0] 000 cd[4:0] 1011011"
ccall                     = "1111111 sel[4:0] cs1[4:0] 000 cd[4:0] 1011011"
-- creturn is a special case of ccall, which would mess up decoding!


-- Register Clearing
clear                     = "1111111 01101 q[1:0] imm[7:5] 000 imm[4:0] 1011011"
fpclear                   = "1111111 10000 q[1:0] imm[7:5] 000 imm[4:0] 1011011"

-- Memory -- Needs further refinement
cmem                      = "0010010 mop[4:0] cb[4:0] 000 cd[4:0] 1011011"

-----------------------------
-- Instruction pretty printer
-----------------------------

-- R-type, 2-operand pretty printer
prettyR_2op instr cs1 cd =
  concat [instr, " ", reg cd, ", ", reg cs1]

-- R-type, 2-operand pretty printer
pretty_reg_clear instr imm qt =
  concat [instr, " ", int qt, ", ", int imm]

cheri_instructions_dissasembly_list = [
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
   , cspecialrw          --> prettyR "cspecialrw"
   , cmove               --> prettyR_2op "cmove"
   , cjalr               --> prettyR_2op "cjalr"
   , ccall               --> prettyR "ccall"
   , clear               --> pretty_reg_clear "clear"
   , fpclear             --> pretty_reg_clear "fpclear"
   , cmem                --> prettyR "cmem"
  ]

-- Instruction pretty printer
pretty :: Integer -> String
pretty instr = 
  decode 32 instr (integer_instructions_dissasembly_list ++ cheri_instructions_dissasembly_list)

genCHERIinspection :: Gen Integer
genCHERIinspection =
  frequency [
      (8,  encode cspecialrw (oneof [return 0x1]) (oneof [return 0x0]) dest)
    , (8,  encode cgetperm src dest)
    , (8,  encode cgettype src dest)
    , (8,  encode cgetbase src dest)
    , (8,  encode cgetlen src dest)
    , (8,  encode cgettag src dest)
    , (8,  encode cgetsealed src dest)
    , (8,  encode cgetoffset src dest)
    , (8,  encode cgetaddr src dest)
  ]

genCHERIarithmatic :: Gen Integer
genCHERIarithmatic =
  frequency [
      (8,  encode cspecialrw (oneof [return 0x1]) (oneof [return 0x0]) dest)
    , (8,  encode addi (geomBits 11 2) src dest)
    , (8,  encode csetoffset src src dest)
    , (8,  encode cincoffset src src dest)
    , (8,  encode csetbounds src src dest)
    , (8,  encode csetboundsexact src src dest)
    , (8,  encode cincoffsetimmediate (bits 12) src dest)
    , (8,  encode csetboundsimmediate (bits 12) src dest)
    , (8,  encode ctoptr     src src dest)
    , (8,  encode cfromptr   src src dest)
    , (8,  encode cmove src dest)
  ]

genCHERImisc :: Gen Integer
genCHERImisc =
  frequency [
      (8,  encode cspecialrw (oneof [return 0x1]) (oneof [return 0x0]) dest)
    , (8,  encode addi (geomBits 11 2) src dest)
    , (8,  encode cincoffsetimmediate (bits 12) src dest)
    , (8,  encode csetboundsimmediate (bits 12) src dest)
    , (8,  encode cseal     src src dest)
    , (8,  encode cunseal   src src dest)
    , (8,  encode candperm  src src dest)
    , (8,  encode cbuildcap src src dest)
    , (8,  encode ccopytype src src dest)
    , (8,  encode ccseal    src src dest)
    , (8,  encode ccleartag src dest)
  ]

genCHERIcontrol :: Gen Integer
genCHERIcontrol =
  frequency [
      (8,  encode cspecialrw (oneof [return 0x1]) (oneof [return 0x0]) dest)
    , (8,  encode addi (geomBits 11 2) src dest)
    , (8,  encode cincoffsetimmediate (bits 12) src dest)
    , (8,  encode csetboundsimmediate (bits 12) src dest)
    , (8,  encode cjalr src dest)
    , (8,  encode ccall src src dest)
  ]
