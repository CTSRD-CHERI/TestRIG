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

module GenCHERI where

import InstrCodec
import Test.QuickCheck
import ISA_Helpers
import RVxxI
import Template
import CHERI

import MemUtils

randomCHERITest :: Template
randomCHERITest = Random $ do {
    remaining <- getSize;
    repeats <- bits 7;
    srcAddr <- src;
    srcData <- src;
    tmpReg <- src;
    tmpReg2 <- src;
    dest <- dest;
    imm <- bits 12;
    mop <- bits 5;
    longImm <- (bits 20);
    fenceOp1 <- (bits 4);
    fenceOp2 <- (bits 4);
    srcScr <- elements [28, 29, 30, 31];
    let test =  Distribution [(if remaining > 10 then 5 else 0, legalLoad),
                              (if remaining > 10 then 5 else 0, legalStore),
                              (if remaining > 10 then 5 else 0, legalCapLoad srcAddr dest),
                              (if remaining > 10 then 5 else 0, legalCapStore srcAddr),
                              (10, uniform $ rvAll srcAddr srcData dest imm longImm fenceOp1 fenceOp2),
                              (10, uniform $ rvCHERIall srcAddr srcData imm mop dest srcScr),
                              (10, Single $ encode cspecialrw srcScr srcAddr dest),
                              (10, switchEncodingMode),
                              (10, cspecialRWChain),
                              (20, randomCCall srcAddr srcData tmpReg tmpReg2),
                              (if remaining > 10 then 1 else 0, surroundWithMemAccess randomCHERITest)] in
        if remaining > 10 then return $ Sequence [test, randomCHERITest] else return test}

randomCCall pccReg idcReg typeReg tmpReg = Random $ do {
    selector <- frequency [(2, return 0), (7, return 1), (1, bits 5)];
    return $ Sequence [
       Distribution [(1, Sequence [Single $ encode addi 0xffd 0 tmpReg, Single $ encode candperm tmpReg pccReg pccReg]), (9, Empty)] -- clear X perm?
     , Distribution [(9, Sequence [Single $ encode addi 0xffd 0 tmpReg, Single $ encode candperm tmpReg idcReg idcReg]), (1, Empty)]
     , Distribution [(1, Sequence [Single $ encode addi 0xeff 0 tmpReg, Single $ encode candperm tmpReg pccReg pccReg]), (9, Empty)] -- clear CCall perm?
     , Distribution [(1, Sequence [Single $ encode addi 0xeff 0 tmpReg, Single $ encode candperm tmpReg idcReg idcReg]), (9, Empty)]
     , Distribution [(9, Single $ encode cseal typeReg pccReg pccReg), (1, Empty)] -- seal?
     , Distribution [(9, Single $ encode cseal typeReg idcReg idcReg), (1, Empty)]
     , Single $ encode ccall idcReg pccReg selector
     , Single $ encode cmove 31 1
]}

legalCapLoad addrReg targetReg = Random $ do {
    tmpReg <- src;
    return $ Sequence [
       Single $ encode andi 0xff addrReg addrReg
     , Single $ encode lui 0x40004 tmpReg
     , Single $ encode slli 1 tmpReg tmpReg
     , Single $ encode add addrReg tmpReg addrReg
     , Single $ encode cload 0x17 addrReg targetReg
]}

legalCapStore addrReg = Random $ do {
    tmpReg <- src;
    dataReg <- dest;
    return $ Sequence [
       Single $ encode andi 0xff addrReg addrReg
     , Single $ encode lui 0x40004 tmpReg
     , Single $ encode slli 1 tmpReg tmpReg
     , Single $ encode add addrReg tmpReg addrReg
     , Single $ encode cstore dataReg addrReg 0x4
  ]}

switchEncodingMode = Random $ do {
     tmpReg1 <- src;
     tmpReg2 <- src;
     mode <- elements [0, 1];
     return $ Sequence [
     Single $ encode cspecialrw 0 0 tmpReg1,
     Single $ encode addi mode 0 tmpReg2,
     Single $ encode csetflags tmpReg2 tmpReg1 tmpReg1,
     Single $ encode cspecialrw 28 tmpReg1 0, --Also write trap vector so we stay in cap mode
     Single $ encode cjalr tmpReg1 0
     ]}

cspecialRWChain = Random $ do {
     tmpReg1 <- src;
     tmpReg2 <- src;
     tmpReg3 <- src;
     tmpReg4 <- src;
     tmpReg5 <- src;
     tmpReg6 <- src;
     return $ Sequence [
     Single $ encode cspecialrw 30 tmpReg1 tmpReg2,
     Single $ encode cjalr      tmpReg2 0,
     Single $ encode cspecialrw 30 tmpReg3 tmpReg4,
     Single $ encode cspecialrw 30 tmpReg5 tmpReg6]}

tagCacheTest :: Template
tagCacheTest = Random $ do {
    addrReg <- src;
    targetReg <- dest;
    return $ Sequence [legalCapStore addrReg, legalCapLoad addrReg targetReg, Single $ encode cgettag targetReg targetReg]}

genCHERIinspection :: Template
genCHERIinspection = Random $ do {
    srcAddr <- src;
    srcData <- src;
    dest <- dest;
    imm <- bits 12;
    longImm <- bits 20;
    fenceOp1 <- bits 3;
    fenceOp2 <- bits 3;
    return $ Distribution[
      (1, uniform $ rvCHERIinspection srcAddr dest)
    , (1, uniform $ rvAll srcAddr srcData dest imm longImm fenceOp1 fenceOp2)]}

genCHERIarithmetic :: Template
genCHERIarithmetic = Random $ do {
    srcAddr <- src;
    srcData <- src;
    dest <- dest;
    imm <- bits 12;
    longImm <- bits 20;
    fenceOp1 <- bits 3;
    fenceOp2 <- bits 3;
    return $ Distribution[
      (1, uniform $ rvCHERIarithmetic srcAddr srcData imm dest)
    , (1, uniform $ rvAll srcAddr srcData dest imm longImm fenceOp1 fenceOp2)]}

genCHERImisc :: Template
genCHERImisc = Random $ do {
    srcAddr <- src;
    srcData <- src;
    dest <- dest;
    imm <- bits 12;
    longImm <- bits 20;
    fenceOp1 <- bits 3;
    fenceOp2 <- bits 3;
    srcScr <- elements [0, 1, 28, 29, 30, 31];
    return $ Distribution[
      (1, uniform $ rvCHERImisc srcAddr srcData imm dest srcScr)
    , (1, uniform $ rvAll srcAddr srcData dest imm longImm fenceOp1 fenceOp2)]}

genCHERIcontrol :: Template
genCHERIcontrol = Random $ do {
    srcAddr <- src;
    srcData <- src;
    dest <- dest;
    imm <- bits 12;
    longImm <- bits 20;
    fenceOp1 <- bits 3;
    fenceOp2 <- bits 3;
    return $ Distribution[
      (1, uniform $ rvCHERIcontrol srcAddr srcData imm dest)
    , (1, uniform $ rvAll srcAddr srcData dest imm longImm fenceOp1 fenceOp2)]}
