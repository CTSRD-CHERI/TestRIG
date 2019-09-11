module GenMulDiv where

import InstrCodec
import Test.QuickCheck
import ISA_Helpers
import RVxxM
import Template
import Prelude hiding (rem, div)

genMulDiv :: Template
genMulDiv = Random $ do
  src1     <- src;
  src2     <- src;
  dest     <- dest;
  return $ uniform [ encode mul    src1 src2 dest
                   , encode mulh   src1 src2 dest
                   , encode mulhsu src1 src2 dest
                   , encode mulhu  src1 src2 dest
                   , encode div    src1 src2 dest
                   , encode divu   src1 src2 dest
                   , encode rem    src1 src2 dest
                   , encode remu   src1 src2 dest
                   ]

genMulDiv64 :: Template
genMulDiv64 = Random $ do
  src1     <- src;
  src2     <- src;
  dest     <- dest;
  return $ uniform [ encode mulw  src1 src2 dest
                   , encode divw  src1 src2 dest
                   , encode divuw src1 src2 dest
                   , encode remw  src1 src2 dest
                   , encode remuw src1 src2 dest
                   ]
