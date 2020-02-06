{-# LANGUAGE ScopedTypeVariables #-}
--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2019 Peter Rugg
-- Copyright (c) 2019, 2020 Alexandre Joannou
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory (Department of Computer Science and
-- Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
-- DARPA SSITH research programme.
--
-- This software was partly developed by the University of Cambridge
-- Computer Laboratory as part of the Partially-Ordered Event-Triggered
-- Systems (POETS) project, funded by EPSRC grant EP/N031768/1.
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

module Templates.Utils.General (
  -- *RISCV pseudo-instructions
  li
, li32
, li64
  -- * Arbitrary value generators
, src
, dest
, csr
, roundingMode
, bits
, exclude
, geomBits
  -- * Memory helpers
  -- ** Basic memory helpers
, memOffset
, loadOp
, loadOp32
, loadOp64
, storeOp
, storeOp32
, storeOp64
  -- ** Advanced memory helpers
, legalLoad
, legalStore
, surroundWithMemAccess
  -- * Other helpers
, prepReg
, prepReg32
, prepReg64
) where

import Test.QuickCheck hiding ((.&.))
import InstrCodec
import Template
import RISCV
import Data.Bits
import Data.Word

-- * RISCV pseudo-instructions
--------------------------------------------------------------------------------

-- | 'li' returns a 'Template' that loads an immediate into a register
li :: ArchDesc -> Integer -> Integer -> Template
li arch reg imm =
  if has_xlen_64 arch
     then Random $ oneof (map return [li32 reg imm, li64 reg imm])
     else li32 reg imm

-- | 'li32' returns a 'Template' that loads a 32-bit immediate into a register
li32 :: Integer -> Integer -> Template
li32 reg imm = instSeq $ doHi20 ++ doLo12
  where doHi20 = [ encode lui (toInteger hi20) reg | hi20 /= 0]
        doLo12 = if (lo12 /= 0 || hi20 == 0)
                    then [encode addi (toInteger lo12) 0 reg] else []
        imm32 :: Word32 = fromInteger imm
        hi20  :: Word32 = ((imm32 + 0x800) `shiftR` 12) .&. 0xfffff
        lo12  :: Word32 = if testBit imm32 12 then imm32 .|. 0xfffff000
                                             else imm32 .&. 0x00000fff

-- | 'li64' returns a 'Template' that loads a 64-bit immediate into a register
li64 :: Integer -> Integer -> Template
li64 reg imm = error "li64 is not yet supported"

-- * Arbitrary value generators
--------------------------------------------------------------------------------

-- | 'inUseReg' generates a register index from the "in use registers",
--   currently one of r0, r1, r2, r3, r4, r16, r17, r18, r19 and r20
inUseReg :: Gen Integer
inUseReg = oneof [choose (0, 4), choose (16, 20)]
-- Alternative implementation:
--   frequency [
--     (32, return 1)
--   , (16, return 2)
--   , (8,  return 3)
--   , (4,  return 4)
--   , (2,  return 5)
--   , (1,  return 0)
--   ]

-- | 'src' generates an arbitrary source register index
src :: Gen Integer
src = inUseReg

-- | 'dest' generates an arbitrary destination register index
dest :: Gen Integer
dest = inUseReg

-- | 'csr' generates an arbitrary csr register index
csr :: Gen Integer
csr = elements $ map fst csrs_map

-- | 'roundingMode' generates a random floating point rounding mode
-- Modes 5 and 6 are reserved for future use in the RISV ISA.
roundingMode :: Gen Integer
roundingMode = oneof $ map return [0, 1, 2, 3, 4, 7]

-- | 'bits' generates an arbitrary integer value of the given bit width
bits :: Int -> Gen Integer
bits w = choose (0, 2^w - 1)

-- | 'exclude' generates an arbitrary value of type 'a' using the provided
--   'Gen a', excluding those present in the provided exclusion list
exclude :: Eq a => [a] -> Gen a -> Gen a
exclude excl orig = orig >>= \x -> if elem x excl then exclude excl orig
                                                  else return x

-- | 'geomBits' generates an arbitrary power of two value, clustered around 1
geomBits :: Int -> Int -> Gen Integer
geomBits hi lo = frequency [(2^(32-i), return (2^i)) | i <- [lo..(hi-1)]]

-- * Memory helpers
--------------------------------------------------------------------------------

-- ** Basic memory helpers

-- | 'memOffset' generates an arbitrary memory address offset
memOffset :: Gen Integer
memOffset = oneof $ map return [0, 1, 64, 65]

-- | 'loadOp' provides a 'Template' for a memory load operation
loadOp :: ArchDesc -> Integer -> Integer -> Template
loadOp arch rs1 rd =
  if has_xlen_64 arch
     then Random $ oneof (map return [loadOp32 rs1 rd, loadOp64 rs1 rd])
     else loadOp32 rs1 rd

-- | 'loadOp32' provides a 'Template' for a RV32I memory load operation
loadOp32 :: Integer -> Integer -> Template
loadOp32 rs1 rd = uniformTemplate $ rv32_i_load rs1 rd 0

-- | 'loadOp64' provides a 'Template' for a rv64 memory load operation
loadOp64 :: Integer -> Integer -> Template
loadOp64 rs1 rd = uniformTemplate $ rv64_i_load rs1 rd 0

-- | 'storeOp' provides a 'Template' for a memory store operation
storeOp :: ArchDesc -> Integer -> Integer -> Template
storeOp arch rs1 rs2 =
  if has_xlen_64 arch
     then Random $ oneof (map return [storeOp32 rs1 rs2, storeOp64 rs1 rs2])
     else storeOp32 rs1 rs2

-- | 'storeOp32' provides a 'Template' for a RV32I memory store operation
storeOp32 :: Integer -> Integer -> Template
storeOp32 rs1 rs2 = uniformTemplate $ rv32_i_store rs1 rs2 0

-- | 'storeOp64' provides a 'Template' for a RV64I memory store operation
storeOp64 :: Integer -> Integer -> Template
storeOp64 rs1 rs2 = uniformTemplate $ rv64_i_store rs1 rs2 0

-- ** Advanced memory helpers

-- | 'legalLoad' provides a 'Template' for a load operation from an arbitrary
--   "RVFI-DII legal" address into an arbitrary register
legalLoad :: ArchDesc -> Template
legalLoad arch = Random $ do
  tmpReg    <- src
  addrReg   <- src
  targetReg <- dest
  return $ instSeq [ encode andi 0xff addrReg addrReg
                   , encode lui 0x40004 tmpReg
                   , encode slli 1 tmpReg tmpReg
                   , encode add addrReg tmpReg addrReg ]
           <> loadOp arch addrReg targetReg

-- | 'legalStore' provides a 'Template' for a store operation from an arbitrary
--   register to an arbitrary "RVFI-DII legal" address
legalStore :: ArchDesc -> Template
legalStore arch = Random $ do
  tmpReg  <- src
  addrReg <- src
  dataReg <- dest
  return $ instSeq [ encode andi 0xff addrReg addrReg
                   , encode lui 0x40004 tmpReg
                   , encode slli 1 tmpReg tmpReg
                   , encode add addrReg tmpReg addrReg ]
           <> storeOp arch dataReg addrReg

-- | 'surroundWithMemAccess' wraps a 'Template' by performing a store operation
--   before it, and following it by a load operation to the same
--   "RVFI-DII legal" address
surroundWithMemAccess :: ArchDesc -> Template -> Template
surroundWithMemAccess arch x = Random $ do
  regAddr <- dest
  regData <- dest
  offset  <- bits 8
  value   <- bits 12
  shift   <- bits 6
  return $    storeToAddress regAddr regData offset value shift
           <> x
           <> loadFromAddress regAddr offset regData
  where loadFromAddress reg offset dest =
          instSeq [ encode lui 0x40004 reg
                  , encode slli 1 reg reg
                  , encode addi offset reg reg ]
          <> loadOp arch reg dest
        storeToAddress regAddr regData offset value shift =
          instSeq [ encode addi value 0 regData
                  , encode slli shift regData regData
                  , encode lui 0x40004 regAddr
                  , encode slli 1 regAddr regAddr
                  , encode addi offset regAddr regAddr ]
          <> storeOp arch regAddr regData

-- * Other helpers
--------------------------------------------------------------------------------

{- No longer used, use 'li32' instead
-- | 'loadImm32' initializes the given register with the given value
loadImm32 dst imm =
  instSeq [ encode addi ((shift imm (-21)) Data.Bits..&. 0x7FF) 0 dst
          , encode slli 11 dst dst
          , encode addi ((shift imm (-10)) Data.Bits..&. 0x7FF) dst dst
          , encode slli 10 dst dst
          , encode addi (imm Data.Bits..&. 0x3FF) dst dst]
-}

-- | 'prepReg' provides a 'Template' to initialize a given register to an
--   arbitrary value
prepReg :: ArchDesc -> Integer -> Template
prepReg arch dst =
  if has_xlen_64 arch
     then Random $ oneof (map return [prepReg32 dst, prepReg64 dst])
     else prepReg32 dst

-- | 'prepReg32' provides a 'Template' to initialize a given register to an
--   arbitrary 32-bit value
prepReg32 :: Integer -> Template
prepReg32 dst = Random $ do imm <- bits 32
                            return (li32 dst imm)

-- | 'prepReg64' provides a 'Template' to initialize a given register to an
--   arbitrary 64-bit value
prepReg64 :: Integer -> Template
prepReg64 dst = replicateTemplate 6 $ Random $ do
  val <- bits 12
  return $ instSeq [ encode slli  12 dst dst
                   , encode xori val dst dst ]
