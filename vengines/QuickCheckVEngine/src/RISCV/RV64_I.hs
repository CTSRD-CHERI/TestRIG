--
-- SPDX-License-Identifier: BSD-2-Clause
--
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

module RISCV.RV64_I (
  rv64_i_disass
, rv64_i
, rv64_i_arith
, rv64_i_load
, rv64_i_store
, rv64_i_mem
, lwu
, ld
, sd
, addiw
, slli64
, srli64
, srai64
, slliw
, srliw
, sraiw
, addw
, subw
, sllw
, srlw
, sraw
) where

import RISCV.Helpers (prettyI, prettyR, prettyL, prettyS)
import InstrCodec (DecodeBranch, (-->), encode)

----------------------
-- RV64_I instructions
----------------------

lwu    = "imm[11:0] rs1[4:0] 110 rd[4:0] 0000011"
ld     = "imm[11:0] rs1[4:0] 011 rd[4:0] 0000011"
sd     = "imm[11:5] rs2[4:0] rs1[4:0] 011 imm[4:0] 0100011"
addiw  = "imm[11:0] rs1[4:0] 000 rd[4:0] 0011011"
slli64 = "000000 imm[5:0] rs1[4:0] 001 rd[4:0] 0010011"
srli64 = "000000 imm[5:0] rs1[4:0] 101 rd[4:0] 0010011"
srai64 = "010000 imm[5:0] rs1[4:0] 101 rd[4:0] 0010011"
slliw  = "0000000 shamt[4:0] rs1[4:0] 001 rd[4:0] 0011011"
srliw  = "0000000 shamt[4:0] rs1[4:0] 101 rd[4:0] 0011011"
sraiw  = "0100000 shamt[4:0] rs1[4:0] 101 rd[4:0] 0011011"
addw   = "0000000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0111011"
subw   = "0100000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0111011"
sllw   = "0000000 rs2[4:0] rs1[4:0] 001 rd[4:0] 0111011"
srlw   = "0000000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0111011"
sraw   = "0100000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0111011"

rv64_i_disass :: [DecodeBranch String]
rv64_i_disass = [ lwu    --> prettyL "lwu"
                , ld     --> prettyL "ld"
                , sd     --> prettyS "sd"
                , addiw  --> prettyI "addiw"
                , slli64 --> prettyI "slli"
                , srli64 --> prettyI "srli"
                , srai64 --> prettyI "srai"
                , slliw  --> prettyI "slliw"
                , srliw  --> prettyI "srliw"
                , sraiw  --> prettyI "sraiw"
                , addw   --> prettyR "addw"
                , subw   --> prettyR "subw"
                , sllw   --> prettyR "sllw"
                , srlw   --> prettyR "srlw"
                , sraw   --> prettyR "sraw"
                ]

rv64_i_arith :: Integer -> Integer -> Integer -> Integer -> [Integer]
rv64_i_arith src1 src2 dest imm = [ encode addiw  imm src1 dest
                                  , encode slli64 imm src1 dest
                                  , encode srli64 imm src1 dest
                                  , encode srai64 imm src1 dest
                                  , encode slliw  imm src1 dest
                                  , encode srliw  imm src1 dest
                                  , encode sraiw  imm src1 dest
                                  , encode addw  src1 src2 dest
                                  , encode subw  src1 src2 dest
                                  , encode sllw  src1 src2 dest
                                  , encode srlw  src1 src2 dest
                                  , encode sraw  src1 src2 dest
                                  ]

rv64_i_load :: Integer -> Integer -> Integer -> [Integer]
rv64_i_load src dest imm = [ encode lwu  imm src dest
                           , encode ld imm src dest
                           ]

rv64_i_store :: Integer -> Integer -> Integer -> [Integer]
rv64_i_store srcAddr srcData imm = [encode sd imm srcData srcAddr]

rv64_i_mem :: Integer -> Integer -> Integer -> Integer -> [Integer] --TODO alignment
rv64_i_mem srcAddr srcData dest imm =
  (rv64_i_load srcAddr dest imm) ++ (rv64_i_store srcAddr srcData imm)

rv64_i :: Integer -> Integer -> Integer -> Integer -> [Integer]
rv64_i srcAddr srcData dest imm =
     rv64_i_arith srcAddr srcData dest imm
  ++ rv64_i_mem srcAddr srcData dest imm
