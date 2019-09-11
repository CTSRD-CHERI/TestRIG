--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Peter Rugg
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

module GenArithmetic where

import InstrCodec
import Test.QuickCheck
import ISA_Helpers
import RVxxI
import Template

genArithmetic :: Template
genArithmetic = Random $ do {
  imm      <- bits 12;
  src1     <- src;
  src2     <- src;
  dest     <- dest;
  longImm  <- bits 20;
  fenceOp1 <- bits 4;
  fenceOp2 <- bits 4;
  offset   <- offset;
  return $ Distribution [
    (8,  Single $ encode add   src1 src2 dest)
  , (8,  Single $ encode slt   src1 src2 dest)
  , (8,  Single $ encode sltu  src1 src2 dest)
  , (8,  Single $ encode andr  src1 src2 dest)
  , (8,  Single $ encode orr   src1 src2 dest)
  , (8,  Single $ encode xorr  src1 src2 dest)
  , (8,  Single $ encode sll   src1 src2 dest)
  , (8,  Single $ encode srl   src1 src2 dest)
  , (8,  Single $ encode sub   src1 src2 dest)
  , (8,  Single $ encode sra   src1 src2 dest)
  , (16, Single $ encode addi  imm src2 dest)
  , (8,  Single $ encode slti  imm src2 dest)
  , (8,  Single $ encode sltiu imm src2 dest)
  , (8,  Single $ encode andi  imm src2 dest)
  , (8,  Single $ encode ori   imm src2 dest)
  , (16, Single $ encode xori  imm src2 dest)
  , (8,  Single $ encode slli  imm src2 dest)
  , (8,  Single $ encode srli  imm src2 dest)
  , (8,  Single $ encode srai  imm src2 dest)
  , (16, Single $ encode lui   longImm dest)
  ]}
