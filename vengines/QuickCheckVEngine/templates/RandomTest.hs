module RandomTest where

import InstrCodec
import Test.QuickCheck
import ISA_Helpers
import RVxxI
import Template

import MemUtils

randomTest :: Template
randomTest = Random $ do {
    remaining <- getSize;
    repeats <- bits 7;
    srcAddr <- src;
    srcData <- src;
    dest <- dest;
    imm <- (bits 12);
    longImm <- (bits 20);
    fenceOp1 <- (bits 4);
    fenceOp2 <- (bits 4);
    let test =  Distribution [(if remaining > 10 then 1 else 0, legalLoad),
                              (if remaining > 10 then 1 else 0, legalStore), 
                              (10, uniform $ rvAll srcAddr srcData dest imm longImm fenceOp1 fenceOp2),
                              (if remaining > 10 then 1 else 0, surroundWithMemAccess randomTest)] in
        if remaining > 10 then return $ Sequence [test, randomTest] else return test}
