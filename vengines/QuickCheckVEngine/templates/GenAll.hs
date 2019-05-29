module GenAll where

import InstrCodec
import Test.QuickCheck
import ISA_Helpers
import RVxxI
import Template

genAll :: Template
genAll = Random $ do {
    imm      <- bits 12;
    src1     <- src;
    src2     <- src;
    dest     <- dest;
    longImm  <- bits 20;
    fenceOp1 <- bits 4;
    fenceOp2 <- bits 4;
    offset   <- offset;
    return $ Distribution [
      (8, Single $ encode addi  imm src1 dest)
    , (8, Single $ encode slti  imm src1 dest)
    , (8, Single $ encode sltiu imm src1 dest)
    , (8, Single $ encode andi  imm src1 dest)
    , (8, Single $ encode ori   imm src1 dest)
    , (8, Single $ encode xori  imm src1 dest)
    , (8, Single $ encode slli  imm src1 dest)
    , (8, Single $ encode srli  imm src1 dest)
    , (8, Single $ encode srai  imm src1 dest)
    , (8, Single $ encode lui   longImm dest)
    , (32,Single $ encode lui   0x80008 dest)
    , (8, Single $ encode auipc longImm dest)
    , (8, Single $ encode jal   longImm dest)
    , (8, Single $ encode jalr  imm src1 dest)
    , (8, Single $ encode beq   imm src1 src2)
    , (8, Single $ encode bne   imm src1 src2)
    , (8, Single $ encode bge   imm src1 src2)
    , (8, Single $ encode bgeu  imm src1 src2)
    , (8, Single $ encode lb    offset src1 dest)
    , (8, Single $ encode lbu   offset src1 dest)
    , (8, Single $ encode lh    offset src1 dest)
    , (8, Single $ encode lhu   offset src1 dest)
    , (8, Single $ encode lw    offset src1 dest)
    , (8, Single $ encode sb    offset src1 src2)
    , (8, Single $ encode sh    offset src1 src2)
    , (8, Single $ encode sw    offset src1 src2)
    , (8, Single $ encode fence fenceOp1 fenceOp2)
    , (8, Single $ encode fence_i)
    ]}
