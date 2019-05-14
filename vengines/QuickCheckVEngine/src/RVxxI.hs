--
-- SPDX-License-Identifier: BSD-2-Clause
--
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

module RVxxI where

import InstrCodec
import Test.QuickCheck
import Control.Monad
import ISA_Helpers

---------------------
-- RV32I instructions
---------------------

add     = ["0000000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0110011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
slt     = ["0000000 rs2[4:0] rs1[4:0] 010 rd[4:0] 0110011", "000000000001 00000 000 rd[4:0] 0010011", "000000000000 00000 000 rd[4:0] 0010011"]
sltu    = ["0000000 rs2[4:0] rs1[4:0] 011 rd[4:0] 0110011", "000000000001 00000 000 rd[4:0] 0010011", "000000000000 00000 000 rd[4:0] 0010011"]
andr    = ["0000000 rs2[4:0] rs1[4:0] 111 rd[4:0] 0110011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
orr     = ["0000000 rs2[4:0] rs1[4:0] 110 rd[4:0] 0110011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
xorr    = ["0000000 rs2[4:0] rs1[4:0] 100 rd[4:0] 0110011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
sll     = ["0000000 rs2[4:0] rs1[4:0] 001 rd[4:0] 0110011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
srl     = ["0000000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
sub     = ["0100000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0110011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
sra     = ["0100000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
addi    = ["imm[11:0] rs1[4:0] 000 rd[4:0] 0010011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
slti    = ["imm[11:0] rs1[4:0] 010 rd[4:0] 0010011", "000000000001 00000 000 rd[4:0] 0010011", "000000000000 00000 000 rd[4:0] 0010011"]
sltiu   = ["imm[11:0] rs1[4:0] 011 rd[4:0] 0010011", "000000000001 00000 000 rd[4:0] 0010011", "000000000000 00000 000 rd[4:0] 0010011"]
andi    = ["imm[11:0] rs1[4:0] 111 rd[4:0] 0010011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
ori     = ["imm[11:0] rs1[4:0] 110 rd[4:0] 0010011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
xori    = ["imm[11:0] rs1[4:0] 100 rd[4:0] 0010011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
slli    = ["0000000 imm[4:0] rs1[4:0] 001 rd[4:0] 0010011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
srli    = ["0000000 imm[4:0] rs1[4:0] 101 rd[4:0] 0010011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
srai    = ["0100000 imm[4:0] rs1[4:0] 101 rd[4:0] 0010011", "000000000000 rs1[4:0] 000 rd[4:0] 0010011"]
lui     = ["imm[19:0] rd[4:0] 0110111"]
auipc   = ["imm[19:0] rd[4:0] 0010111"]
jal     = ["imm[19:19] imm[9:0] imm[10:10] imm[18:11] rd[4:0] 1101111", "00000000000000000000 rd[4:0] 0010111"]
jalr    = ["imm[11:0] rs1[4:0] 000 rd[4:0] 1100111", "00000000000000000000 rd[4:0] 0010111"]
beq     = ["im[11:11] im[9:4] rs2[4:0] rs1[4:0] 000 im[3:0] im[10:10] 1100011"]
bne     = ["im[11:11] im[9:4] rs2[4:0] rs1[4:0] 001 im[3:0] im[10:10] 1100011"]
blt     = ["im[11:11] im[9:4] rs2[4:0] rs1[4:0] 100 im[3:0] im[10:10] 1100011"]
bltu    = ["im[11:11] im[9:4] rs2[4:0] rs1[4:0] 110 im[3:0] im[10:10] 1100011"]
bge     = ["im[11:11] im[9:4] rs2[4:0] rs1[4:0] 101 im[3:0] im[10:10] 1100011"]
bgeu    = ["im[11:11] im[9:4] rs2[4:0] rs1[4:0] 111 im[3:0] im[10:10] 1100011"]
lb      = ["imm[11:0] rs1[4:0] 000 rd[4:0] 0000011"]
lbu     = ["imm[11:0] rs1[4:0] 100 rd[4:0] 0000011"]
lh      = ["imm[11:0] rs1[4:0] 001 rd[4:0] 0000011"]
lhu     = ["imm[11:0] rs1[4:0] 101 rd[4:0] 0000011"]
lw      = ["imm[11:0] rs1[4:0] 010 rd[4:0] 0000011"]
sb      = ["imm[11:5] rs2[4:0] rs1[4:0] 000 imm[4:0] 0100011"]
sh      = ["imm[11:5] rs2[4:0] rs1[4:0] 001 imm[4:0] 0100011"]
sw      = ["imm[11:5] rs2[4:0] rs1[4:0] 010 imm[4:0] 0100011"]
fence   = ["0000 pred[3:0] succ[3:0] 00000 000 00000 0001111"]
fence_i = ["0000 0000 0000 00000 001 00000 0001111"]
resrvd  = ["0000 0000 0000 00000 000 00000 0000000"]

integer_instructions_dissasembly_list :: [DecodeBranch String]
integer_instructions_dissasembly_list = [
    head(add)     --> prettyR "add"
  , head(slt)     --> prettyR "slt"
  , head(sltu)    --> prettyR "sltu"
  , head(andr)    --> prettyR "and"
  , head(orr)     --> prettyR "or"
  , head(xorr)    --> prettyR "xor"
  , head(sll)     --> prettyR "sll"
  , head(srl)     --> prettyR "srl"
  , head(sub)     --> prettyR "sub"
  , head(sra)     --> prettyR "sra"
  , head(addi)    --> prettyI "addi"
  , head(slti)    --> prettyI "slti"
  , head(sltiu)   --> prettyI "sltiu"
  , head(andi)    --> prettyI "andi"
  , head(ori)     --> prettyI "ori"
  , head(xori)    --> prettyI "xori"
  , head(slli)    --> prettyI "slli"
  , head(srli)    --> prettyI "srli"
  , head(srai)    --> prettyI "srai"
  , head(lui)     --> prettyU "lui"
  , head(auipc)   --> prettyU "auipc"
  , head(jal)     --> prettyU "jal"
  , head(jalr)    --> prettyI "jalr"
  , head(beq)     --> prettyB "beq"
  , head(bne)     --> prettyB "bne"
  , head(blt)     --> prettyB "blt"
  , head(bltu)    --> prettyB "bltu"
  , head(bge)     --> prettyB "bge"
  , head(bgeu)    --> prettyB "bgeu"
  , head(lb)      --> prettyL "lb"
  , head(lbu)     --> prettyL "lbu"
  , head(lh)      --> prettyL "lh"
  , head(lhu)     --> prettyL "lhu"
  , head(lw)      --> prettyL "lw"
  , head(sb)      --> prettyS "sb"
  , head(sh)      --> prettyS "sh"
  , head(sw)      --> prettyS "sw"
  , head(fence)   --> prettyF
  , head(fence_i) --> "fence_i"
  , head(resrvd)  --> "reserved"
  ]
 
genAll :: Gen [Integer]
genAll =
  frequency [
    (8,  encode addi  (bits 12) src dest)
  , (8,  encode slti  (bits 12) src dest)
  , (8,  encode sltiu (bits 12) src dest)
  , (8,  encode andi  (bits 12) src dest)
  , (8,  encode ori   (bits 12) src dest)
  , (8,  encode xori  (bits 12) src dest)
  , (8,  encode slli  (bits 12) src dest)
  , (8,  encode srli  (bits 12) src dest)
  , (8,  encode srai  (bits 12) src dest)
  , (8,  encode lui   (bits 20) dest)
  , (32, encode lui   (oneof [return 0x80008]) dest)
  , (8,  encode auipc (bits 20) dest)
  , (8,  encode jal   (bits 20) dest)
  , (8,  encode jalr  (bits 12) src dest)
  , (8,  encode beq   (bits 12) src src)
  , (8,  encode bne   (bits 12) src src)
  , (8,  encode bge   (bits 12) src src)
  , (8,  encode bgeu  (bits 12) src src)
  , (8,  encode lb    offset src dest)
  , (8,  encode lbu   offset src dest)
  , (8,  encode lh    offset src dest)
  , (8,  encode lhu   offset src dest)
  , (8,  encode lw    offset src dest)
  , (8,  encode sb    offset src src)
  , (8,  encode sh    offset src src)
  , (8,  encode sw    offset src src)
  , (8,  encode fence (bits 4) (bits 4))
  , (8,  encode fence_i)
  ]

genArithmetic :: Gen [Integer]
genArithmetic =
  frequency [
    (8,  encode add   src src dest)
  , (8,  encode slt   src src dest)
  , (8,  encode sltu  src src dest)
  , (8,  encode andr  src src dest)
  , (8,  encode orr   src src dest)
  , (8,  encode xorr  src src dest)
  , (8,  encode sll   src src dest)
  , (8,  encode srl   src src dest)
  , (8,  encode sub   src src dest)
  , (8,  encode sra   src src dest)
  , (16, encode addi  (bits 12) src dest)
  , (8,  encode slti  (bits 12) src dest)
  , (8,  encode sltiu (bits 12) src dest)
  , (8,  encode andi  (bits 12) src dest)
  , (8,  encode ori   (bits 12) src dest)
  , (16, encode xori  (bits 12) src dest)
  , (8,  encode slli  (bits 12) src dest)
  , (8,  encode srli  (bits 12) src dest)
  , (8,  encode srai  (bits 12) src dest)
  , (16, encode lui   (bits 20) dest)
  ]

genMemory :: Gen [Integer]
genMemory =
  frequency [
    (8,  encode addi (geomBits 11 0) src dest)
  , (8,  encode ori  (geomBits 11 0) src dest)
  , (16, encode lui  (oneof [return 0x80008]) dest)
  , (8,  encode lb    (geomBits 11 0) src dest)
  , (8,  encode lbu   (geomBits 11 0) src dest)
  , (8,  encode lh    (geomBits 11 1) src dest)
  , (8,  encode lhu   (geomBits 11 1) src dest)
  , (8,  encode lw    (geomBits 11 2) src dest)
  , (8,  encode sb    (geomBits 11 0) src src)
  , (8,  encode sh    (geomBits 11 1) src src)
  , (8,  encode sw    (geomBits 11 2) src src)
  , (2,  encode fence (bits 4) (bits 4))
  , (2,  encode fence_i)
  ]

genControlFlow :: Gen [Integer]
genControlFlow =
  frequency [
    (8,  encode addi (geomBits 11 2) src dest)
  , (8,  encode ori  (geomBits 11 2) src dest)
  , (8,  encode auipc (bits 20) dest)
  , (8,  encode jal   (bits 20) dest)
  , (8,  encode jalr  (bits 12) src dest)
  , (8,  encode beq   (bits 12) src src)
  , (8,  encode bne   (bits 12) src src)
  , (8,  encode bge   (bits 12) src src)
  , (8,  encode bgeu  (bits 12) src src)
  ]
