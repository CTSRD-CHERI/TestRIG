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

module MemUtils where

import InstrCodec
import Test.QuickCheck
import ISA_Helpers
import RVxxI
import Template

loadOp :: Integer -> Integer -> Template
loadOp reg dest = uniform $ rvLoad reg dest 0

storeOp :: Integer -> Integer -> Template
storeOp regAddr regData = uniform $ rvStore regAddr regData 0

storeToAddress :: Integer -> Integer -> Integer -> Integer -> Integer -> Template
storeToAddress regAddr regData offset value shift = Sequence [
    Single $ encode addi value 0 regData
  , Single $ encode slli shift regData regData
  , Single $ encode lui 0x40004 regAddr
  , Single $ encode slli 1 regAddr regAddr
  , Single $ encode addi offset regAddr regAddr
  , storeOp regAddr regData]

loadFromAddress :: Integer -> Integer -> Integer -> Template
loadFromAddress reg offset dest = Sequence [
    Single $ encode lui 0x40004 reg
  , Single $ encode slli 1 reg reg
  , Single (encode addi offset reg reg)
  , loadOp reg dest]

surroundWithMemAccess :: Template -> Template
surroundWithMemAccess x = Random $ do {
                                   regAddr <- dest;
                                   regData <- dest;
                                   offset <- bits 8;
                                   value <- bits 12;
                                   shift <- bits 6;
                                   return $ Sequence [storeToAddress regAddr regData offset value shift, x, loadFromAddress regAddr offset regData]}

legalLoad :: Template
legalLoad = Random $ do {
    tmpReg <- src;
    addrReg <- src;
    targetReg <- dest;
    return $ Sequence [
       Single $ encode andi 0xff addrReg addrReg
     , Single $ encode lui 0x40004 tmpReg
     , Single $ encode slli 1 tmpReg tmpReg
     , Single $ encode add addrReg tmpReg addrReg
     , loadOp addrReg targetReg
]}

legalStore :: Template
legalStore = Random $ do {
    tmpReg <- src;
    addrReg <- src;
    dataReg <- dest;
    return $ Sequence [
       Single $ encode andi 0xff addrReg addrReg
     , Single $ encode lui 0x40004 tmpReg
     , Single $ encode slli 1 tmpReg tmpReg
     , Single $ encode add addrReg tmpReg addrReg
     , storeOp dataReg addrReg
]}
