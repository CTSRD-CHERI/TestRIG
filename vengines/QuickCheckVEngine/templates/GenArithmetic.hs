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
