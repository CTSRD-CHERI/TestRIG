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
