--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019, 2020 Alexandre Joannou
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

module Templates.GenFP (
  gen_rv32_f
, gen_rv64_f
, gen_rv32_d
, gen_rv64_d
) where

import RISCV.RV32_F
import RISCV.RV64_F
import RISCV.RV32_D
import RISCV.RV64_D
import Template
import Templates.Utils

gen_rv32_f :: Template
gen_rv32_f = genFP False False

gen_rv64_f :: Template
gen_rv64_f = genFP False True

gen_rv32_d :: Template
gen_rv32_d = genFP True False

gen_rv64_d :: Template
gen_rv64_d = genFP True True

genFP :: Bool -> Bool -> Template
genFP has_d has_xlen_64 = Random $ do
  src1 <- src
  src2 <- src
  src3 <- src
  dest <- dest
  rm   <- bits 3
  imm  <- bits 12
  let insts = rv32_f src1 src2 src3 dest rm imm
              ++ if has_xlen_64 then rv64_f src1 dest rm else []
              ++ if has_d then rv32_d src1 src2 src3 dest rm imm else []
              ++ if has_d && has_xlen_64 then rv64_d src1 dest rm else []
  return $ uniformTemplate insts
