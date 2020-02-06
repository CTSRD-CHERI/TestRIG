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

module RISCV.RV32_F (
  rv32_f_disass
, rv32_f
, flw
, fsw
, fmadd_s
, fmsub_s
, fnmsub_s
, fnmadd_s
, fadd_s
, fsub_s
, fmul_s
, fdiv_s
, fsqrt_s
, fsgnj_s
, fsgnjn_s
, fsgnjx_s
, fmin_s
, fmax_s
, fcvt_w_s
, fcvt_wu_s
, fmv_x_w
, feq_s
, flt_s
, fle_s
, fclass_s
, fcvt_s_w
, fcvt_s_wu
, fmv_w_x
) where

import RISCV.Helpers (prettyR, prettyS, prettyR4_rm, prettyR_rm,
                      prettyR_FI_1op, prettyR_FF_1op, prettyR_IF_1op,
                      prettyR_FF_1op_rm, prettyR_FI_1op_rm, prettyR_IF_1op_rm)
import InstrCodec (DecodeBranch, (-->), encode)

----------------------
-- RV32_F instructions
----------------------

flw       = "imm[11:0]            rs1[4:0]     010  rd[4:0] 0000111"
fsw       = "imm[11:5]   rs2[4:0] rs1[4:0]     010 imm[4:0] 0100111"
fmadd_s   = "rs3[4:0] 00 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1000011"
fmsub_s   = "rs3[4:0] 00 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1000111"
fnmsub_s  = "rs3[4:0] 00 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1001011"
fnmadd_s  = "rs3[4:0] 00 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1001111"
fadd_s    = "0000000     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fsub_s    = "0000100     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fmul_s    = "0001000     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fdiv_s    = "0001100     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fsqrt_s   = "0101100        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fsgnj_s   = "0010000     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fsgnjn_s  = "0010000     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
fsgnjx_s  = "0010000     rs2[4:0] rs1[4:0]     010  rd[4:0] 1010011"
fmin_s    = "0010100     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fmax_s    = "0010100     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
fcvt_w_s  = "1100000        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_wu_s = "1100000        00001 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fmv_x_w   = "1110000        00000 rs1[4:0]     000  rd[4:0] 1010011"
feq_s     = "1010000     rs2[4:0] rs1[4:0]     010  rd[4:0] 1010011"
flt_s     = "1010000     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
fle_s     = "1010000     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fclass_s  = "1110000        00000 rs1[4:0]     001  rd[4:0] 1010011"
fcvt_s_w  = "1101000        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_s_wu = "1101000        00001 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fmv_w_x   = "1111000        00000 rs1[4:0]     000  rd[4:0] 1010011"

rv32_f_disass :: [DecodeBranch String]
rv32_f_disass = [ flw       --> prettyR           "flw"
                , fsw       --> prettyS           "fsw"
                , fmadd_s   --> prettyR4_rm       "fmadd.s"
                , fmsub_s   --> prettyR4_rm       "fmsub.s"
                , fnmsub_s  --> prettyR4_rm       "fnmsub.s"
                , fnmadd_s  --> prettyR4_rm       "fnmadd.s"
                , fadd_s    --> prettyR_rm        "fadd.s"
                , fsub_s    --> prettyR_rm        "fsub.s"
                , fmul_s    --> prettyR_rm        "fmul.s"
                , fdiv_s    --> prettyR_rm        "fdiv.s"
                , fsqrt_s   --> prettyR_FF_1op_rm "fsqrt.s"
                , fsgnj_s   --> prettyR           "fsgnj.s"
                , fsgnjn_s  --> prettyR           "fsgnjn.s"
                , fsgnjx_s  --> prettyR           "fsgnjx.s"
                , fmin_s    --> prettyR           "fmin.s"
                , fmax_s    --> prettyR           "fmax.s"
                , fcvt_w_s  --> prettyR_IF_1op_rm "fcvt.w.s"
                , fcvt_wu_s --> prettyR_FI_1op_rm "fcvt.wu.s"
                , fmv_x_w   --> prettyR_IF_1op    "fmv.x.w"
                , feq_s     --> prettyR           "feq.s"
                , flt_s     --> prettyR           "flt.s"
                , fle_s     --> prettyR           "fle.s"
                , fclass_s  --> prettyR_IF_1op    "fclass.s"
                , fcvt_s_w  --> prettyR_FI_1op_rm "fcvt.s.w"
                , fcvt_s_wu --> prettyR_FI_1op_rm "fcvt.s.wu"
                , fmv_w_x   --> prettyR_FI_1op    "fmv.w.x"
                ]

rv32_f :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
       -> [Integer]
rv32_f src1 src2 src3 dest rm imm = [ encode flw        imm      src1    dest
                                    , encode fsw        imm src2 src1    dest
                                    , encode fmadd_s   src3 src2 src1 rm dest
                                    , encode fmsub_s   src3 src2 src1 rm dest
                                    , encode fnmsub_s  src3 src2 src1 rm dest
                                    , encode fnmadd_s  src3 src2 src1 rm dest
                                    , encode fadd_s         src2 src1 rm dest
                                    , encode fsub_s         src2 src1 rm dest
                                    , encode fmul_s         src2 src1 rm dest
                                    , encode fdiv_s         src2 src1 rm dest
                                    , encode fsqrt_s             src1 rm dest
                                    , encode fsgnj_s        src2 src1    dest
                                    , encode fsgnjn_s       src2 src1    dest
                                    , encode fsgnjx_s       src2 src1    dest
                                    , encode fmin_s         src2 src1    dest
                                    , encode fmax_s         src2 src1    dest
                                    , encode fcvt_w_s            src1 rm dest
                                    , encode fcvt_wu_s           src1 rm dest
                                    , encode fmv_x_w             src1    dest
                                    , encode feq_s          src2 src1    dest
                                    , encode flt_s          src2 src1    dest
                                    , encode fle_s          src2 src1    dest
                                    , encode fclass_s            src1    dest
                                    , encode fcvt_s_w            src1 rm dest
                                    , encode fcvt_s_wu           src1 rm dest
                                    , encode fmv_w_x             src1    dest
                                    ]
