--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2019 Peter Rugg
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

module RISCV.RV32_I (
  rv32_i_disass
, rv32_i
, rv32_i_arith
, rv32_i_ctrl
, rv32_i_load
, rv32_i_store
, rv32_i_fence
, rv32_i_mem
, add
, slt
, sltu
, and
, or
, xor
, sll
, srl
, sub
, sra
, addi
, slti
, sltiu
, andi
, ori
, xori
, slli
, srli
, srai
, lui
, auipc
, jal
, jalr
, beq
, bne
, blt
, bltu
, bge
, bgeu
, lb
, lbu
, lh
, lhu
, lw
, sb
, sh
, sw
, fence
, resrvd
, mret
, ecall
, ebreak
) where

import RISCV.Helpers (prettyR, prettyI, prettyU, prettyB, prettyF, prettyS, prettyL)
import InstrCodec (DecodeBranch, (-->), encode)
import Prelude hiding (and, or)

----------------------
-- RV32_I instructions
----------------------

add    = "0000000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0110011"
slt    = "0000000 rs2[4:0] rs1[4:0] 010 rd[4:0] 0110011"
sltu   = "0000000 rs2[4:0] rs1[4:0] 011 rd[4:0] 0110011"
and    = "0000000 rs2[4:0] rs1[4:0] 111 rd[4:0] 0110011"
or     = "0000000 rs2[4:0] rs1[4:0] 110 rd[4:0] 0110011"
xor    = "0000000 rs2[4:0] rs1[4:0] 100 rd[4:0] 0110011"
sll    = "0000000 rs2[4:0] rs1[4:0] 001 rd[4:0] 0110011"
srl    = "0000000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011"
sub    = "0100000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0110011"
sra    = "0100000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011"
addi   = "imm[11:0] rs1[4:0] 000 rd[4:0] 0010011"
slti   = "imm[11:0] rs1[4:0] 010 rd[4:0] 0010011"
sltiu  = "imm[11:0] rs1[4:0] 011 rd[4:0] 0010011"
andi   = "imm[11:0] rs1[4:0] 111 rd[4:0] 0010011"
ori    = "imm[11:0] rs1[4:0] 110 rd[4:0] 0010011"
xori   = "imm[11:0] rs1[4:0] 100 rd[4:0] 0010011"
slli   = "0000000 imm[4:0] rs1[4:0] 001 rd[4:0] 0010011"
srli   = "0000000 imm[4:0] rs1[4:0] 101 rd[4:0] 0010011"
srai   = "0100000 imm[4:0] rs1[4:0] 101 rd[4:0] 0010011"
lui    = "imm[19:0] rd[4:0] 0110111"
auipc  = "imm[19:0] rd[4:0] 0010111"
jal    = "imm[19:19] imm[9:0] imm[10:10] imm[18:11] rd[4:0] 1101111"
jalr   = "imm[11:0] rs1[4:0] 000 rd[4:0] 1100111"
beq    = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 000 im[3:0] im[10:10] 1100011"
bne    = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 001 im[3:0] im[10:10] 1100011"
blt    = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 100 im[3:0] im[10:10] 1100011"
bltu   = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 110 im[3:0] im[10:10] 1100011"
bge    = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 101 im[3:0] im[10:10] 1100011"
bgeu   = "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 111 im[3:0] im[10:10] 1100011"
lb     = "imm[11:0] rs1[4:0] 000 rd[4:0] 0000011"
lbu    = "imm[11:0] rs1[4:0] 100 rd[4:0] 0000011"
lh     = "imm[11:0] rs1[4:0] 001 rd[4:0] 0000011"
lhu    = "imm[11:0] rs1[4:0] 101 rd[4:0] 0000011"
lw     = "imm[11:0] rs1[4:0] 010 rd[4:0] 0000011"
sb     = "imm[11:5] rs2[4:0] rs1[4:0] 000 imm[4:0] 0100011"
sh     = "imm[11:5] rs2[4:0] rs1[4:0] 001 imm[4:0] 0100011"
sw     = "imm[11:5] rs2[4:0] rs1[4:0] 010 imm[4:0] 0100011"
fence  = "0000 pred[3:0] succ[3:0] 00000 000 00000 0001111"
resrvd = "0000 0000 0000 00000 000 00000 0000000"
mret   = "0011 0000 0010 00000 000 00000 1110011"
ecall  = "000000000000 00000 000 00000 1110011"
ebreak = "000000000001 00000 000 00000 1110011"

rv32_i_disass :: [DecodeBranch String]
rv32_i_disass = [ add    --> prettyR "add"
                , slt    --> prettyR "slt"
                , sltu   --> prettyR "sltu"
                , and    --> prettyR "and"
                , or     --> prettyR "or"
                , xor    --> prettyR "xor"
                , sll    --> prettyR "sll"
                , srl    --> prettyR "srl"
                , sub    --> prettyR "sub"
                , sra    --> prettyR "sra"
                , addi   --> prettyI "addi"
                , slti   --> prettyI "slti"
                , sltiu  --> prettyI "sltiu"
                , andi   --> prettyI "andi"
                , ori    --> prettyI "ori"
                , xori   --> prettyI "xori"
                , slli   --> prettyI "slli"
                , srli   --> prettyI "srli"
                , srai   --> prettyI "srai"
                , lui    --> prettyU "lui"
                , auipc  --> prettyU "auipc"
                , jal    --> prettyU "jal"
                , jalr   --> prettyI "jalr"
                , beq    --> prettyB "beq"
                , bne    --> prettyB "bne"
                , blt    --> prettyB "blt"
                , bltu   --> prettyB "bltu"
                , bge    --> prettyB "bge"
                , bgeu   --> prettyB "bgeu"
                , lb     --> prettyL "lb"
                , lbu    --> prettyL "lbu"
                , lh     --> prettyL "lh"
                , lhu    --> prettyL "lhu"
                , lw     --> prettyL "lw"
                , sb     --> prettyS "sb"
                , sh     --> prettyS "sh"
                , sw     --> prettyS "sw"
                , fence  --> prettyF
                , resrvd --> "reserved"
                , mret   --> "mret"
                , ecall  --> "ecall"
                , ebreak --> "ebreak"
                ]

rv32_i_arith :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_i_arith src1 src2 dest imm longImm = [ encode add  src1 src2 dest
                                          , encode slt  src1 src2 dest
                                          , encode sltu src1 src2 dest
                                          , encode and  src1 src2 dest
                                          , encode or   src1 src2 dest
                                          , encode xor  src1 src2 dest
                                          , encode sll  src1 src2 dest
                                          , encode srl  src1 src2 dest
                                          , encode sub  src1 src2 dest
                                          , encode sra  src1 src2 dest
                                          , encode addi  imm src1 dest
                                          , encode slti  imm src1 dest
                                          , encode sltiu imm src1 dest
                                          , encode andi  imm src1 dest
                                          , encode ori   imm src1 dest
                                          , encode xori  imm src1 dest
                                          , encode slli  imm src1 dest
                                          , encode srli  imm src1 dest
                                          , encode srai  imm src1 dest
                                          , encode lui   longImm  dest
                                          ]

rv32_i_ctrl :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_i_ctrl src1 src2 dest imm longImm = [ encode auipc longImm dest
                                         , encode jal   longImm dest
                                         , encode jalr  imm src1 dest
                                         , encode beq   imm src1 src2
                                         , encode bne   imm src1 src2
                                         , encode bge   imm src1 src2
                                         , encode bgeu  imm src1 src2
                                         ]

rv32_i_load :: Integer -> Integer -> Integer -> [Integer]
rv32_i_load src dest imm = [ encode lb  imm src dest
                           , encode lbu imm src dest
                           , encode lh  imm src dest
                           , encode lhu imm src dest
                           , encode lw  imm src dest
                           ]

rv32_i_store :: Integer -> Integer -> Integer -> [Integer]
rv32_i_store srcAddr srcData imm = [ encode sb imm srcData srcAddr
                                   , encode sh imm srcData srcAddr
                                   , encode sw imm srcData srcAddr
                                   ]

rv32_i_fence :: Integer -> Integer -> [Integer]
rv32_i_fence fenceOp1 fenceOp2 = [encode fence fenceOp1 fenceOp2]

rv32_i_mem :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [Integer] --TODO alignment
rv32_i_mem srcAddr srcData dest imm fenceOp1 fenceOp2 =
     (rv32_i_load srcAddr dest imm)
  ++ (rv32_i_store srcAddr srcData imm)
  ++ (rv32_i_fence fenceOp1 fenceOp2)

rv32_i :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_i srcAddr srcData dest imm longImm fenceOp1 fenceOp2 =
     (rv32_i_arith srcAddr srcData dest imm longImm)
  ++ (rv32_i_mem srcAddr srcData dest imm fenceOp1 fenceOp2)
  ++ (rv32_i_ctrl srcAddr srcData dest imm longImm)
