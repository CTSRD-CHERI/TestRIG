--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018 Matthew Naylor
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

module ISA_Helpers where

import InstrCodec
import Test.QuickCheck
import Control.Monad

-----------------------------
-- Instruction pretty printer
-----------------------------

-- Register pretty printer
reg :: Integer -> String
reg i = "r" ++ show i

-- Integer pretty printer
int :: Integer -> String
int i = show i

-- R-type pretty printer
prettyR instr rs2 rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, ", ", reg rs2]

-- I-type pretty printer
prettyI instr imm rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, ", ", int imm]

-- Pretty printer for loads
prettyL instr imm rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, "[", int imm, "]"]

-- S-type pretty printer
prettyS instr imm rs2 rs1 =
  concat [instr, " ", reg rs2, ", ", reg rs1, "[", int imm, "]"]

-- U-type pretty printer
prettyU instr imm rd =
  concat [instr, " ", reg rd, ", ", int imm]

-- B-type pretty printer
prettyB instr imm rs2 rs1 =
  concat [instr, " ", reg rs1, ", ", reg rs2, ", ", int imm]

-- Pretty printer for fence instruction
prettyF pred succ =
  concat ["fence ", int pred, ", ", int succ]

-- R-type, 2-operand pretty printer
prettyR_2op instr cs1 cd =
  concat [instr, " ", reg cd, ", ", reg cs1]

-------------------------------
-- Random instruction generator
-------------------------------

-- Generate random destination register
-- Use 6 registers with a geometic distribution
dest :: Gen Integer
dest = choose (0, 5)
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
src = choose (0, 5)

-- Generate random integer with given bit-width
bits :: Int -> Gen Integer
bits w = choose (0, 2^w - 1)

-- Generate but exclude some patterns
exclude :: [Integer] -> Gen Integer -> Gen Integer
exclude excl orig = do attempt <- orig; if elem attempt excl then exclude excl orig else return attempt

-- Power of two values clustered around 1.
geomBits :: Int -> Int -> Gen Integer
geomBits hi lo = frequency [(2^(32-i), return (2^i))| i <- [lo..(hi-1)]]

-- Generate memory offset
offset :: Gen Integer
offset = oneof [return 0, return 1, return 64, return 65]
