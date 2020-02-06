--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Peter Rugg
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

module Templates.GenCHERI (
  randomCHERITest
) where

import Test.QuickCheck
import RISCV
import InstrCodec
import Template
import Templates.Utils

genRandomCHERITest :: ArchDesc -> Gen Template
genRandomCHERITest arch = do
  remaining <- getSize
  repeats   <- bits 7
  srcAddr   <- src
  srcData   <- src
  tmpReg    <- src
  tmpReg2   <- src
  dest      <- dest
  imm       <- bits 12
  mop       <- bits 5
  longImm   <- (bits 20)
  fenceOp1  <- (bits 4)
  fenceOp2  <- (bits 4)
  csrAddr   <- frequency [(1, return 0xbc0), (1, return 0x342), (1, bits 12)]
  srcScr    <- elements [28, 29, 30, 31]
  thisNested <- resize (remaining `Prelude.div` 2) (genRandomCHERITest arch)
  let test =  Distribution [ (if remaining > 10 then 5 else 0, legalLoad arch)
                           , (if remaining > 10 then 5 else 0, legalStore arch)
                           , (if remaining > 10 then 5 else 0, legalCapLoad srcAddr dest)
                           , (if remaining > 10 then 5 else 0, legalCapStore srcAddr)
                           , (10, uniformTemplate $ rv32_i srcAddr srcData dest imm longImm fenceOp1 fenceOp2) -- TODO add csr
                           , (10, uniformTemplate $ rv32_xcheri srcAddr srcData srcScr imm mop dest)
                           , (10, Single $ encode cspecialrw srcScr srcAddr dest)
                           , (10, switchEncodingMode)
                           , (10, cspecialRWChain)
                           , (20, randomCCall srcAddr srcData tmpReg tmpReg2)
                           , (if remaining > 10 then 1 else 0, surroundWithMemAccess arch thisNested) ]
  if remaining > 10
    then do nextNested <- resize (remaining `Prelude.div` 2) (genRandomCHERITest arch)
            return $ test <> nextNested
    else return test

randomCHERITest :: ArchDesc -> Template
randomCHERITest arch = Random $ genRandomCHERITest arch
