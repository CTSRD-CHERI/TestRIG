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

module Templates.Utils where

import Test.QuickCheck (Gen, choose, frequency, oneof, elements)
import InstrCodec
import Template
import RISCV.Helpers
import RISCV.RV32_I
import RISCV.RV32_Zicsr
import Data.Bits

-- Generate random destination register
-- Use 6 registers with a geometic distribution
dest :: Gen Integer
dest = oneof [choose (0, 4), choose (16, 20)]
-- dest =
--   frequency [
--     (32, return 1)
--   , (16, return 2)
--   , (8,  return 3)
--   , (4,  return 4)
--   , (2,  return 5)
--   , (1,  return 0)
--   ]

-- Generate random source register
-- Use 6 registers with a uniform distribution
src :: Gen Integer
src = oneof [choose (0, 4), choose (16, 20)]

-- Generate random csr register
-- Use 6 registers with a uniform distribution
csr :: Gen Integer
csr = elements $ map fst csrs_map

-- Generate random integer with given bit-width
bits :: Int -> Gen Integer
bits w = choose (0, 2^w - 1)

-- Generate but exclude some patterns
exclude :: [Integer] -> Gen Integer -> Gen Integer
exclude excl orig = do attempt <- orig; if elem attempt excl then exclude excl orig else return attempt

-- Power of two values clustered around 1.
geomBits :: Int -> Int -> Gen Integer
geomBits hi lo = frequency [(2^(32-i), return (2^i)) | i <- [lo..(hi-1)]]

-- Generate memory offset
memOffset :: Gen Integer
memOffset = oneof [return 0, return 1, return 64, return 65]

-- Memory load Template
loadOp :: Integer -> Integer -> Template
loadOp reg dest = uniformTemplate $ rv32_i_load reg dest 0

-- Memory store Template
storeOp :: Integer -> Integer -> Template
storeOp regAddr regData = uniformTemplate $ rv32_i_store regAddr regData 0

storeToAddress :: Integer -> Integer -> Integer -> Integer -> Integer -> Template
storeToAddress regAddr regData offset value shift =
  instSeq [ encode addi value 0 regData
          , encode slli shift regData regData
          , encode lui 0x40004 regAddr
          , encode slli 1 regAddr regAddr
          , encode addi offset regAddr regAddr ]
  <> storeOp regAddr regData

loadFromAddress :: Integer -> Integer -> Integer -> Template
loadFromAddress reg offset dest =
  instSeq [ encode lui 0x40004 reg
          , encode slli 1 reg reg
          , encode addi offset reg reg ]
  <> loadOp reg dest

surroundWithMemAccess :: Template -> Template
surroundWithMemAccess x = Random $ do
  regAddr <- dest
  regData <- dest
  offset  <- bits 8
  value   <- bits 12
  shift   <- bits 6
  return $    storeToAddress regAddr regData offset value shift
           <> x
           <> loadFromAddress regAddr offset regData

legalLoad :: Template
legalLoad = Random $ do
  tmpReg    <- src
  addrReg   <- src
  targetReg <- dest
  return $ instSeq [ encode andi 0xff addrReg addrReg
                   , encode lui 0x40004 tmpReg
                   , encode slli 1 tmpReg tmpReg
                   , encode add addrReg tmpReg addrReg ]
           <> loadOp addrReg targetReg

legalStore :: Template
legalStore = Random $ do
  tmpReg  <- src
  addrReg <- src
  dataReg <- dest
  return $ instSeq [ encode andi 0xff addrReg addrReg
                   , encode lui 0x40004 tmpReg
                   , encode slli 1 tmpReg tmpReg
                   , encode add addrReg tmpReg addrReg ]
           <> storeOp dataReg addrReg

loadImm32 dst imm = Sequence [ Single $ encode addi ((shift imm (-21)) Data.Bits..&. 0x7FF) 0 dst
                             , Single $ encode slli 11 dst dst
                             , Single $ encode addi ((shift imm (-10)) Data.Bits..&. 0x7FF) dst dst
                             , Single $ encode slli 10 dst dst
                             , Single $ encode addi (imm Data.Bits..&. 0x3FF) dst dst]

prepReg32 :: Integer -> Template
prepReg32 dst = Random $ do imm <- bits 32
                            return (loadImm32 dst imm)

prepReg64 :: Integer -> Template
prepReg64 dst = replicateTemplate 6 $ Random $ do
  val <- bits 12
  return $ instSeq [ encode slli  12 dst dst
                   , encode xori val dst dst ]
