--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Peter Rugg
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

module Templates.GenAll where

import InstrCodec
import Test.QuickCheck
import RISCV.ArchDesc
import RISCV.RV32_I
import RISCV.RV32_M
import RISCV.RV32_A
import RISCV.RV32_F
import RISCV.RV32_D
import RISCV.RV32_Zifencei
import RISCV.RV32_Zicsr
import RISCV.RV32_Xcheri
import RISCV.RV64_I
import RISCV.RV64_M
import RISCV.RV64_A
import RISCV.RV64_F
import RISCV.RV64_D
import Template
import Templates.Utils

genAll :: ArchDesc -> Template
genAll desc = Random $
  do imm     <- bits 12
     src1    <- src
     src2    <- src
     src3    <- src
     dest    <- dest
     longImm <- bits 20
     fOp1    <- bits 4
     fOp2    <- bits 4
     aq      <- bits 1
     rl      <- bits 1
     rm      <- bits 3
     mop     <- bits 5
     uimm    <- bits 5
     offset  <- memOffset
     srcScr  <- elements [28, 29, 30, 31]
     let insts = [[ (8, uniform (rv32_i_arith src1 src2 dest imm longImm))
                  , (8, uniform (rv32_i_ctrl src1 src2 dest imm longImm))
                  , (8, uniform (rv32_i_mem src1 src2 dest offset fOp1 fOp2))
                  , (32, Single $ encode lui 0x80008 dest)
                  ] | has_i desc]
              ++ [[ (8, uniform (rv64_i_arith src1 src2 dest imm))
                  , (8, uniform (rv64_i_mem src1 src2 dest offset))
                  ] | has_i desc && has_xlen_64 desc]
              ++ [[ (8, uniform (rv32_m src1 src2 imm))
                  ] | has_m desc]
              ++ [[ (8, uniform (rv64_m src1 src2 imm))
                  ] | has_m desc && has_xlen_64 desc]
              ++ [[ (8, uniform (rv32_a src1 src2 dest aq rl))
                  ] | has_a desc]
              ++ [[ (8, uniform (rv64_a src1 src2 dest aq rl))
                  ] | has_a desc && has_xlen_64 desc]
              ++ [[ (8, uniform (rv32_f src1 src2 src3 dest rm imm))
                  ] | has_f desc]
              ++ [[ (8, uniform (rv64_f src1 dest rm))
                  ] | has_f desc && has_xlen_64 desc]
              ++ [[ (8, uniform (rv32_d src1 src2 src3 dest rm imm))
                  ] | has_d desc]
              ++ [[ (8, uniform (rv64_d src1 dest rm))
                  ] | has_d desc && has_xlen_64 desc]
              ++ [[ (8, uniform rv32_zifencei)
                  ] | has_ifencei desc]
              ++ [[ (8, uniform (rv32_zicsr src1 dest imm uimm))
                  ] | has_icsr desc]
              ++ [[ (8, uniform (rv32_xcheri src1 src2 srcScr imm mop dest))
                  ] | has_cheri desc]
     return $ Distribution (concat insts)
