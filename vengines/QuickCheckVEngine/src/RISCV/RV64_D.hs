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

module RISCV.RV64_D (
  rv64_d_disass
, rv64_d
, fcvt_l_d
, fcvt_lu_d
, fmv_x_d
, fcvt_d_l
, fcvt_d_lu
, fmv_d_x
) where

import RISCV.Helpers (prettyR_FI_1op_rm, prettyR_IF_1op_rm, prettyR_FI_1op, prettyR_IF_1op)
import InstrCodec (DecodeBranch, (-->), encode)

----------------------
-- RV64_F instructions
----------------------

fcvt_l_d  = "1100001    00010 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fcvt_lu_d = "1100001    00011 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fmv_x_d   = "1110001    00000 rs1[4:0]     000 rd[4:0] 1010011"
fcvt_d_l  = "1101001    00010 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fcvt_d_lu = "1101001    00011 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fmv_d_x   = "1111001    00000 rs1[4:0]     000 rd[4:0] 1010011"

rv64_d_disass :: [DecodeBranch String]
rv64_d_disass = [ fcvt_l_d  --> prettyR_FI_1op_rm "fcvt.l.d"
                , fcvt_lu_d --> prettyR_IF_1op_rm "fcvt.lu.d"
                , fmv_x_d   --> prettyR_IF_1op    "fmv.x.d"
                , fcvt_d_l  --> prettyR_FI_1op_rm "fcvt.d.l"
                , fcvt_d_lu --> prettyR_FI_1op_rm "fcvt.d.lu"
                , fmv_d_x   --> prettyR_FI_1op    "fmv.d.x"
                ]

rv64_d :: Integer -> Integer -> Integer -> [Integer]
rv64_d src1 dest rm = [ encode fcvt_l_d  src1 rm dest
                      , encode fcvt_lu_d src1 rm dest
                      , encode fmv_x_d   src1    dest
                      , encode fcvt_d_l  src1 rm dest
                      , encode fcvt_d_lu src1 rm dest
                      , encode fmv_d_x   src1    dest
                      ]
