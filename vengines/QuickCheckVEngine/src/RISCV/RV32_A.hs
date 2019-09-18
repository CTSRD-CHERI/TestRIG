--
-- SPDX-License-Identifier: BSD-2-Clause
--
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

module RISCV.RV32_A (
  rv32_a_disass
, rv32_a
) where

import RISCV.Helpers (prettyR_A_1op, prettyR_A)
import InstrCodec (DecodeBranch, (-->), encode)

----------------------
-- RV32_A instructions
----------------------

lr_w      = "00010 aq[0] rl[0]    00000 rs1[4:0] 010 rd[4:0] 0101111"
sc_w      = "00011 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amoswap_w = "00001 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amoadd_w  = "00000 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amoxor_w  = "00100 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amoand_w  = "01100 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amoor_w   = "01000 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amomin_w  = "10000 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amomax_w  = "10100 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amominu_w = "11000 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amomaxu_w = "11100 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"

rv32_a_disass :: [DecodeBranch String]
rv32_a_disass = [ lr_w      --> prettyR_A_1op "lr.w"
                , sc_w      --> prettyR_A "sc.w"
                , amoswap_w --> prettyR_A "amoswap.w"
                , amoadd_w  --> prettyR_A "amoadd.w"
                , amoxor_w  --> prettyR_A "amoxor.w"
                , amoand_w  --> prettyR_A "amoand.w"
                , amoor_w   --> prettyR_A "amoor.w"
                , amomin_w  --> prettyR_A "amomin.w"
                , amomax_w  --> prettyR_A "amomax.w"
                , amominu_w --> prettyR_A "amominu.w"
                , amomaxu_w --> prettyR_A "amomaxu.w"
                ]

rv32_a :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_a src1 src2 dest aq rl = [ encode lr_w      aq rl src2 src1 dest
                              , encode sc_w      aq rl src2 src1 dest
                              , encode amoswap_w aq rl src2 src1 dest
                              , encode amoadd_w  aq rl src2 src1 dest
                              , encode amoxor_w  aq rl src2 src1 dest
                              , encode amoand_w  aq rl src2 src1 dest
                              , encode amoor_w   aq rl src2 src1 dest
                              , encode amomin_w  aq rl src2 src1 dest
                              , encode amomax_w  aq rl src2 src1 dest
                              , encode amominu_w aq rl src2 src1 dest
                              , encode amomaxu_w aq rl src2 src1 dest
                              ]
