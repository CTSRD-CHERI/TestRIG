module GenControlFlow where

import InstrCodec
import Test.QuickCheck
import ISA_Helpers
import RVxxI
import Template

genControlFlow :: Template
genControlFlow = Random $ do {
  imm      <- bits 12;
  src1     <- src;
  src2     <- src;
  dest     <- dest;
  longImm  <- bits 20;
  fenceOp1 <- bits 4;
  fenceOp2 <- bits 4;
  offset   <- geomBits 11 2;
  return $ Distribution [
    (8, Single $ encode addi  offset src1 dest)
  , (8, Single $ encode ori   offset src1 dest)
  , (8, Single $ encode auipc longImm dest)
  , (8, Single $ encode jal   longImm dest)
  , (8, Single $ encode jalr  imm src1 dest)
  , (8, Single $ encode beq   imm src1 src2)
  , (8, Single $ encode bne   imm src1 src2)
  , (8, Single $ encode bge   imm src1 src2)
  , (8, Single $ encode bgeu  imm src1 src2)
  ]}
