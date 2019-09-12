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

module RISCV.RV64_M (
  rv64_m_disass
, rv64_m
, mulw
, divw
, divuw
, remw
, remuw
) where

import RISCV.Helpers (prettyR)
import InstrCodec (DecodeBranch, (-->), encode)

----------------------
-- RV64_M instructions
----------------------

mulw  = "0000001 rs2[4:0] rs1[4:0] 000 rd[4:0] 0111011"
divw  = "0000001 rs2[4:0] rs1[4:0] 100 rd[4:0] 0111011"
divuw = "0000001 rs2[4:0] rs1[4:0] 101 rd[4:0] 0111011"
remw  = "0000001 rs2[4:0] rs1[4:0] 110 rd[4:0] 0111011"
remuw = "0000001 rs2[4:0] rs1[4:0] 111 rd[4:0] 0111011"

rv64_m_disass :: [DecodeBranch String]
rv64_m_disass = [ mulw  --> prettyR "mulw"
                , divw  --> prettyR "divw"
                , divuw --> prettyR "divuw"
                , remw  --> prettyR "remw"
                , remuw --> prettyR "remuw"
                ]

rv64_m :: Integer -> Integer -> Integer -> [Integer]
rv64_m src1 src2 dest = [ encode mulw  src1 src2 dest
                        , encode divw  src1 src2 dest
                        , encode divuw src1 src2 dest
                        , encode remw  src1 src2 dest
                        , encode remuw src1 src2 dest
                        ]
