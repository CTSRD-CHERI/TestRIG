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

module RISCV.RV64_A (
  rv64_a_disass
, rv64_a
, lr_d
, sc_d
, amoswap_d
, amoadd_d
, amoxor_d
, amoand_d
, amoor_d
, amomin_d
, amomax_d
, amominu_d
, amomaxu_d
) where

import RISCV.Helpers (prettyR_A_1op, prettyR_A)
import InstrCodec (DecodeBranch, (-->), encode)

----------------------
-- RV64_A instructions
----------------------

lr_d      = "00010 aq[0] rl[0]    00000 rs1[4:0] 011 rd[4:0] 0101111"
sc_d      = "00011 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amoswap_d = "00001 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amoadd_d  = "00000 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amoxor_d  = "00100 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amoand_d  = "01100 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amoor_d   = "01000 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amomin_d  = "10000 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amomax_d  = "10100 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amominu_d = "11000 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amomaxu_d = "11100 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"

rv64_a_disass :: [DecodeBranch String]
rv64_a_disass = [ lr_d      --> prettyR_A_1op "lr.d"
                , sc_d      --> prettyR_A "sc.d"
                , amoswap_d --> prettyR_A "amoswap.d"
                , amoadd_d  --> prettyR_A "amoadd.d"
                , amoxor_d  --> prettyR_A "amoxor.d"
                , amoand_d  --> prettyR_A "amoand.d"
                , amoor_d   --> prettyR_A "amoor.d"
                , amomin_d  --> prettyR_A "amomin.d"
                , amomax_d  --> prettyR_A "amomax.d"
                , amominu_d --> prettyR_A "amominu.d"
                , amomaxu_d --> prettyR_A "amomaxu.d"
                ]

rv64_a :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv64_a src1 src2 dest aq rl = [ encode lr_d      aq rl src2 src1 dest
                              , encode sc_d      aq rl src2 src1 dest
                              , encode amoswap_d aq rl src2 src1 dest
                              , encode amoadd_d  aq rl src2 src1 dest
                              , encode amoxor_d  aq rl src2 src1 dest
                              , encode amoand_d  aq rl src2 src1 dest
                              , encode amoor_d   aq rl src2 src1 dest
                              , encode amomin_d  aq rl src2 src1 dest
                              , encode amomax_d  aq rl src2 src1 dest
                              , encode amominu_d aq rl src2 src1 dest
                              , encode amomaxu_d aq rl src2 src1 dest
                              ]
