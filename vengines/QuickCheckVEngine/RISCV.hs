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

module RISCV where

import InstrCodec
import Test.QuickCheck
import Control.Monad

---------------------
-- RV32I instructions
---------------------

add     = "0000000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0110011"
slt     = "0000000 rs2[4:0] rs1[4:0] 010 rd[4:0] 0110011"
sltu    = "0000000 rs2[4:0] rs1[4:0] 011 rd[4:0] 0110011"
andr    = "0000000 rs2[4:0] rs1[4:0] 111 rd[4:0] 0110011"
orr     = "0000000 rs2[4:0] rs1[4:0] 110 rd[4:0] 0110011"
xorr    = "0000000 rs2[4:0] rs1[4:0] 100 rd[4:0] 0110011"
sll     = "0000000 rs2[4:0] rs1[4:0] 001 rd[4:0] 0110011"
srl     = "0000000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011"
sub     = "0100000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0110011"
sra     = "0100000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011"
addi    = "imm[11:0] rs1[4:0] 000 rd[4:0] 0010011"
slti    = "imm[11:0] rs1[4:0] 010 rd[4:0] 0010011"
sltiu   = "imm[11:0] rs1[4:0] 011 rd[4:0] 0010011"
andi    = "imm[11:0] rs1[4:0] 111 rd[4:0] 0010011"
ori     = "imm[11:0] rs1[4:0] 110 rd[4:0] 0010011"
xori    = "imm[11:0] rs1[4:0] 100 rd[4:0] 0010011"
slli    = "0000000 imm[4:0] rs1[4:0] 001 rd[4:0] 0010011"
srli    = "0000000 imm[4:0] rs1[4:0] 101 rd[4:0] 0010011"
srai    = "0100000 imm[4:0] rs1[4:0] 101 rd[4:0] 0010011"
lui     = "imm[19:0] rd[4:0] 0110111"
auipc   = "imm[19:0] rd[4:0] 0010111"
jal     = "imm[19:19] imm[9:0] imm[10:10] imm[18:11] rd[4:0] 1101111"
jalr    = "imm[11:0] rs1[4:0] 000 rd[4:0] 1100111"
beq     = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 000 im[3:0] im[10:10] 1100011"
bne     = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 001 im[3:0] im[10:10] 1100011"
blt     = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 100 im[3:0] im[10:10] 1100011"
bltu    = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 110 im[3:0] im[10:10] 1100011"
bge     = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 101 im[3:0] im[10:10] 1100011"
bgeu    = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 111 im[3:0] im[10:10] 1100011"
lb      = "imm[11:0] rs1[4:0] 000 rd[4:0] 0000011"
lbu     = "imm[11:0] rs1[4:0] 100 rd[4:0] 0000011"
lh      = "imm[11:0] rs1[4:0] 001 rd[4:0] 0000011"
lhu     = "imm[11:0] rs1[4:0] 101 rd[4:0] 0000011"
lw      = "imm[11:0] rs1[4:0] 010 rd[4:0] 0000011"
sb      = "imm[11:5] rs2[4:0] rs1[4:0] 000 imm[4:0] 0100011"
sh      = "imm[11:5] rs2[4:0] rs1[4:0] 001 imm[4:0] 0100011"
sw      = "imm[11:5] rs2[4:0] rs1[4:0] 010 imm[4:0] 0100011"
fence   = "0000 pred[3:0] succ[3:0] 00000 000 00000 0001111"
fence_i = "0000 0000 0000 00000 001 00000 0001111"
resrvd  = "0000 0000 0000 00000 000 00000 0000000"

-----------------------------
-- Instruction pretty printer
-----------------------------

-- Register pretty printer
reg :: Integer -> String
reg i = "r" ++ show i

-- Integer pretty printer
int :: Integer -> String
int i = show i

-- R-type pretty printer
prettyR instr rs2 rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, ", ", reg rs2]

-- I-type pretty printer
prettyI instr imm rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, ", ", int imm]

-- Pretty printer for loads
prettyL instr imm rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, "[", int imm, "]"]

-- S-type pretty printer
prettyS instr imm rs2 rs1 =
  concat [instr, " ", reg rs2, ", ", reg rs1, "[", int imm, "]"]

-- U-type pretty printer
prettyU instr imm rd =
  concat [instr, " ", reg rd, ", ", int imm]

-- B-type pretty printer
prettyB instr imm rs2 rs1 =
  concat [instr, " ", reg rs1, ", ", reg rs2, ", ", int imm]

-- Pretty printer for fence instruction
prettyF pred succ =
  concat ["fence ", int pred, ", ", int succ]

-- Instruction pretty printer
pretty :: Integer -> String
pretty instr = 
  decode 32 instr [
    add     --> prettyR "add"
  , slt     --> prettyR "slt"
  , sltu    --> prettyR "sltu"
  , andr    --> prettyR "and"
  , orr     --> prettyR "or"
  , xorr    --> prettyR "xor"
  , sll     --> prettyR "sll"
  , srl     --> prettyR "srl"
  , sub     --> prettyR "sub"
  , sra     --> prettyR "sra"
  , addi    --> prettyI "addi"
  , slti    --> prettyI "slti"
  , sltiu   --> prettyI "sltiu"
  , andi    --> prettyI "andi"
  , ori     --> prettyI "ori"
  , xori    --> prettyI "xori"
  , slli    --> prettyI "slli"
  , srli    --> prettyI "srli"
  , srai    --> prettyI "srai"
  , lui     --> prettyU "lui"
  , auipc   --> prettyU "auipc"
  , jal     --> prettyU "jal"
  , jalr    --> prettyI "jalr"
  , beq     --> prettyB "beq"
  , bne     --> prettyB "bne"
  , blt     --> prettyB "blt"
  , bltu    --> prettyB "bltu"
  , bge     --> prettyB "bge"
  , bgeu    --> prettyB "bgeu"
  , lb      --> prettyL "lb"
  , lbu     --> prettyL "lbu"
  , lh      --> prettyL "lh"
  , lhu     --> prettyL "lhu"
  , lw      --> prettyL "lw"
  , sb      --> prettyS "sb"
  , sh      --> prettyS "sh"
  , sw      --> prettyS "sw"
  , fence   --> prettyF
  , fence_i --> "fence_i"
  , resrvd  --> "reserved"
  ]

-------------------------------
-- Random instruction generator
-------------------------------

-- Generate random destination register
-- Use 6 registers with a geometic distribution
dest :: Gen Integer
dest = choose (0, 5)
-- dest =
--   frequency [
--     (32, return 1)
--   , (16, return 2)
--   , (8,  return 3)
--   , (4,  return 4)
--   , (2,  return 5)
--   , (1,  return 0)
--   ]

-- Generate random source register
-- Use 6 registers with a uniform distribution
src :: Gen Integer
src = choose (0, 5)

-- Generate random integer with given bit-width
bits :: Int -> Gen Integer
bits w = choose (0, 2^w - 1)

-- Power of two values clustered around 1.
geomBits :: Int -> Int -> Gen Integer
geomBits hi lo = frequency [(2^(32-i), return (2^i))| i <- [lo..(hi-1)]]

-- Generate memory offset
offset :: Gen Integer
offset = oneof [return 0, return 1, return 64, return 65]
 
genAll :: Gen Integer
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
  , (32,  encode lui   (bits 20) dest)
  , (32,  encode auipc (bits 20) dest)
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

genArithmetic :: Gen Integer
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

genMemory :: Gen Integer
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

genControlFlow :: Gen Integer
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
