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

module GenMemory where

import InstrCodec
import Test.QuickCheck
import ISA_Helpers
import RVxxI
import Template

genMemory :: Template
genMemory = Random $ do {
  imm      <- bits 12;
  src1     <- src;
  src2     <- src;
  dest     <- dest;
  longImm  <- bits 20;
  fenceOp1 <- bits 4;
  fenceOp2 <- bits 4;
  offset   <- geomBits 11 0;
  return $ Distribution [
    (8,  Single $ encode addi  offset src1 dest)
  , (8,  Single $ encode ori   offset src1 dest)
  , (16, Single $ encode lui   0x80008 dest)
  , (8,  Single $ encode lb    offset src1 dest)
  , (8,  Single $ encode lbu   offset src1 dest)
  , (8,  Single $ encode lh    offset src1 dest)
  , (8,  Single $ encode lhu   offset src1 dest)
  , (8,  Single $ encode lw    offset src1 dest)
  , (8,  Single $ encode sb    offset src1 src2)
  , (8,  Single $ encode sh    offset src1 src2)
  , (8,  Single $ encode sw    offset src1 src2)
  , (2,  Single $ encode fence fenceOp1 fenceOp2 )
  , (2,  Single $ encode fence_i)
  ]}
