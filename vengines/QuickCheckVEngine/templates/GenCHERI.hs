
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
                              (10, cspecialRWChain),
                              (if remaining > 10 then 1 else 0, surroundWithMemAccess randomCHERITest)] in
        if remaining > 10 then return $ Sequence [test, randomCHERITest] else return test}


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
