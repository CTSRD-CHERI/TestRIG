--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Peter Rugg
-- Copyright (c) 2020 Alexandre Joannou
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

module Templates.RandomTest where

import Template
import Templates.Utils
import Test.QuickCheck
import RISCV.RV32_I
import Templates.Utils

genRandomTest :: Gen Template
genRandomTest = do
  remaining <- getSize
  repeats   <- bits 7
  srcAddr   <- src
  srcData   <- src
  dest      <- dest
  imm       <- (bits 12)
  longImm   <- (bits 20)
  fenceOp1  <- (bits 4)
  fenceOp2  <- (bits 4)
  csrAddr   <- frequency [ (1, return 0xbc0), (1, return 0x342), (1, bits 12) ]
  thisNested <- resize (remaining `div` 2) genRandomTest
  let test = Distribution [ (if remaining > 10 then 1 else 0, legalLoad)
                          , (if remaining > 10 then 1 else 0, legalStore)
                          , (10, uniformTemplate $ rv32_i srcAddr srcData dest imm longImm fenceOp1 fenceOp2) --TODO re-add csrs
                          , (if remaining > 10 then 1 else 0, surroundWithMemAccess thisNested) ]
  if remaining > 10
    then do nextNested <- resize (remaining `div` 2) genRandomTest
            return $ test <> nextNested
    else return test

randomTest :: Template
randomTest = Random genRandomTest
