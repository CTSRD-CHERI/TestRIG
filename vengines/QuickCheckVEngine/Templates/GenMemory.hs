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

module Templates.GenMemory (
  gen_rv32_i_memory
, gen_rv32_i_zifencei_memory
, gen_rv64_i_memory
, gen_rv64_i_zifencei_memory
) where

import InstrCodec
import Test.QuickCheck
import RISCV.RV32_I
import RISCV.RV32_Zifencei
import Template
import Templates.Utils

gen_rv32_i_memory :: Template
gen_rv32_i_memory = gen_memory False False

gen_rv32_i_zifencei_memory :: Template
gen_rv32_i_zifencei_memory = gen_memory True False

gen_rv64_i_memory :: Template
gen_rv64_i_memory = gen_memory False True

gen_rv64_i_zifencei_memory :: Template
gen_rv64_i_zifencei_memory = gen_memory True True

gen_memory :: Bool -> Bool -> Template
gen_memory has_zifencei has_xlen_64 = Random $
  do imm      <- bits 12
     src1     <- src
     src2     <- src
     dest     <- dest
     fenceOp1 <- bits 4
     fenceOp2 <- bits 4
     offset   <- geomBits 11 0
     let insts = [ (8,  Single $ encode addi  offset src1 dest)
                 , (8,  Single $ encode ori   offset src1 dest)
                 , (16, Sequence ( [Single ( encode lui   0x40004 dest ), Single ( encode slli 1 dest dest )]) )
                 , (8, uniform $ rv32_i_load  src1 dest offset)
                 , (8, uniform $ rv32_i_store src1 src2 offset)
                 , (2, uniform $ rv32_i_fence fenceOp1 fenceOp2)
                 ]
                 ++ if has_zifencei then [(2,  Single $ encode fence_i)] else []
                 ++ if has_xlen_64 then [] else [] -- TODO
     return $ Distribution insts
