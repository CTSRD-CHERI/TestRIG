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
