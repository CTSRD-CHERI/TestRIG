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

module RISCV.RV32_D (
  rv32_d_disass
, rv32_d
, fld
, fsd
, fmadd_d
, fmsub_d
, fnmsub_d
, fnmadd_d
, fadd_d
, fsub_d
, fmul_d
, fdiv_d
, fsqrt_d
, fsgnj_d
, fsgnjn_d
, fsgnjx_d
, fmin_d
, fmax_d
, fcvt_s_d
, fcvt_d_s
, feq_d
, flt_d
, fle_d
, fclass_d
, fcvt_w_d
, fcvt_wu_d
, fcvt_d_w
, fcvt_d_wu
) where

import RISCV.Helpers (prettyR, prettyS, prettyR4_rm, prettyR_rm,
                      prettyR_IF_1op, prettyR_FF_1op_rm, prettyR_FI_1op_rm,
                      prettyR_IF_1op_rm)
import InstrCodec (DecodeBranch, (-->), encode)

----------------------
-- RV32_D instructions
----------------------

fld       = "imm[11:0]            rs1[4:0]     011  rd[4:0] 0000111"
fsd       = "imm[11:5]   rs2[4:0] rs1[4:0]     011 imm[4:0] 0100111"
fmadd_d   = "rs3[4:0] 01 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1000011"
fmsub_d   = "rs3[4:0] 01 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1000111"
fnmsub_d  = "rs3[4:0] 01 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1001011"
fnmadd_d  = "rs3[4:0] 01 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1001111"
fadd_d    = "0000001     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fsub_d    = "0000101     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fmul_d    = "0001001     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fdiv_d    = "0001101     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fsqrt_d   = "0101101        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fsgnj_d   = "0010001     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fsgnjn_d  = "0010001     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
fsgnjx_d  = "0010001     rs2[4:0] rs1[4:0]     010  rd[4:0] 1010011"
fmin_d    = "0010101     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fmax_d    = "0010101     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
fcvt_s_d  = "0100000        00001 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_d_s  = "0100001        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
feq_d     = "1010001     rs2[4:0] rs1[4:0]     010  rd[4:0] 1010011"
flt_d     = "1010001     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
fle_d     = "1010001     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fclass_d  = "1110001        00000 rs1[4:0]     001  rd[4:0] 1010011"
fcvt_w_d  = "1100001        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_wu_d = "1100001        00001 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_d_w  = "1101001        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_d_wu = "1101001        00001 rs1[4:0] rm[2:0]  rd[4:0] 1010011"

rv32_d_disass :: [DecodeBranch String]
rv32_d_disass = [ fld       --> prettyR           "fld"
                , fsd       --> prettyS           "fsd"
                , fmadd_d   --> prettyR4_rm       "fmadd.d"
                , fmsub_d   --> prettyR4_rm       "fmsub.d"
                , fnmsub_d  --> prettyR4_rm       "fnmsub.d"
                , fnmadd_d  --> prettyR4_rm       "fnmadd.d"
                , fadd_d    --> prettyR_rm        "fadd.d"
                , fsub_d    --> prettyR_rm        "fsub.d"
                , fmul_d    --> prettyR_rm        "fmul.d"
                , fdiv_d    --> prettyR_rm        "fdiv.d"
                , fsqrt_d   --> prettyR_FF_1op_rm "fsqrt.d"
                , fsgnj_d   --> prettyR           "fsgnj.d"
                , fsgnjn_d  --> prettyR           "fsgnjn.d"
                , fsgnjx_d  --> prettyR           "fsgnjx.d"
                , fmin_d    --> prettyR           "fmin.d"
                , fmax_d    --> prettyR           "fmax.d"
                , fcvt_s_d  --> prettyR_FF_1op_rm "fcvt.s.d"
                , fcvt_d_s  --> prettyR_FF_1op_rm "fcvt.d.s"
                , feq_d     --> prettyR           "feq.d"
                , flt_d     --> prettyR           "flt.d"
                , fle_d     --> prettyR           "fle.d"
                , fclass_d  --> prettyR_IF_1op    "fclass.d"
                , fcvt_w_d  --> prettyR_IF_1op_rm "fcvt.w.d"
                , fcvt_wu_d --> prettyR_IF_1op_rm "fcvt.wu.d"
                , fcvt_d_w  --> prettyR_FI_1op_rm "fcvt.d.w"
                , fcvt_d_wu --> prettyR_FI_1op_rm "fcvt.d.wu"
                ]

rv32_d :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
       -> [Integer]
rv32_d src1 src2 src3 dest rm imm = [ encode fld        imm      src1    dest
                                    , encode fsd        imm src2 src1    dest
                                    , encode fmadd_d   src3 src2 src1 rm dest
                                    , encode fmsub_d   src3 src2 src1 rm dest
                                    , encode fnmsub_d  src3 src2 src1 rm dest
                                    , encode fnmadd_d  src3 src2 src1 rm dest
                                    , encode fadd_d         src2 src1 rm dest
                                    , encode fsub_d         src2 src1 rm dest
                                    , encode fmul_d         src2 src1 rm dest
                                    , encode fdiv_d         src2 src1 rm dest
                                    , encode fsqrt_d             src1 rm dest
                                    , encode fsgnj_d        src2 src1    dest
                                    , encode fsgnjn_d       src2 src1    dest
                                    , encode fsgnjx_d       src2 src1    dest
                                    , encode fmin_d         src2 src1    dest
                                    , encode fmax_d         src2 src1    dest
                                    , encode fcvt_s_d            src1 rm dest
                                    , encode fcvt_d_s            src1 rm dest
                                    , encode feq_d          src2 src1    dest
                                    , encode flt_d          src2 src1    dest
                                    , encode fle_d          src2 src1    dest
                                    , encode fclass_d            src1    dest
                                    , encode fcvt_w_d            src1 rm dest
                                    , encode fcvt_wu_d           src1 rm dest
                                    , encode fcvt_d_w            src1 rm dest
                                    , encode fcvt_d_wu           src1 rm dest
                                    ]
