--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Hesham Almatary 
-- Copyright (c) 2018 Matthew Naylor
-- All rights reserved.
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

module CHERI where

import InstrCodec
import Test.QuickCheck
import Control.Monad
import RISCV

---------------------
-- CHERI instructions
---------------------

cgetperm                  = "1111111 00000 cs1[4:0] 000 rd[4:0] 1011011"
cgettype                  = "1111111 00001 cs1[4:0] 000 rd[4:0] 1011011"
cgetbase                  = "1111111 00010 cs1[4:0] 000 rd[4:0] 1011011"
cgetlen                   = "1111111 00011 cs1[4:0] 000 rd[4:0] 1011011"
cgettag                   = "1111111 00100 cs1[4:0] 000 rd[4:0] 1011011"
cgetsealed                = "1111111 00101 cs1[4:0] 000 rd[4:0] 1011011"
cgetoffset                = "1111111 00110 cs1[4:0] 000 rd[4:0] 1011011"
cgetaddr                  = "1111111 01111 cs1[4:0] 000 rd[4:0] 1011011"

cseal                     = "0001011 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cunseal                   = "0001100 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
candperm                  = "0001101 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetoffset                = "0001111 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cincoffset                = "0010001 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetbounds                = "0001000 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetboundsexact           = "0001001 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cbuildcap                 = "0011101 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
ccopytype                 = "0011110 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
ccseal                    = "0011111 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
ccleartag                 = "1111111 01011 cs1[4:0] 000 cd[4:0] 1011011"
cincoffsetimmediate       = "imm[11:0] cs1[4:0] 001 cd[4:0] 1011011"
csetboundsimmediate       = "imm[11:0] cs1[4:0] 002 cd[4:0] 1011011"

-----------------------------
-- Instruction pretty printer
-----------------------------

-- Register pretty printer
reg :: Integer -> String
reg i = "c" ++ show i

-- Integer pretty printer
int :: Integer -> String
int i = show i

-- R-type pretty printer
prettyRC instr cs2 cs1 cd =
  concat [instr, " ", reg cd, ", ", reg cs1, ", ", reg cs2]

-- R-type pretty printer
prettyR_2op instr cs1 cd =
  concat [instr, " ", reg cd, ", ", reg cs1]

-- I-type pretty printer
prettyIC instr imm cs1 cd =
  concat [instr, " ", reg cd, ", ", reg cs1, ", ", int imm]

cheri_instructions_dissasembly_list = [
	   cgetperm            --> prettyR "cgetperm"
	 , cgettype            --> prettyR "cgettype"
	 , cgetbase            --> prettyR "cgetbase"
	 , cgetlen             --> prettyR "cgetlen"
	 , cgettag             --> prettyR "cgettag"
	 , cgetsealed          --> prettyR "cgetsealed"
	 , cgetoffset          --> prettyR "cgetoffset"
	 , cgetaddr            --> prettyR "cgetaddr"
	 , cseal               --> prettyR "cseal"
	 , cunseal             --> prettyR "cunseal"
	 , candperm            --> prettyR "candperm"
	 , csetoffset          --> prettyR "csetoffset"
	 , cincoffset          --> prettyR "cincoffset"
	 , csetbounds          --> prettyR "csetbounds"
	 , csetboundsexact     --> prettyR "csetboundsexact"
	 , cbuildcap           --> prettyR "cbuildcap"
	 , ccopytype           --> prettyR "ccopytype"
	 , ccseal              --> prettyR "ccseal"
	 , ccleartag           --> prettyR_2op "ccleartag"
   , cincoffsetimmediate --> prettyI "cincoffsetimmediate"
   , csetboundsimmediate --> prettyI "csetboundsimmediate"
  ]

-- Instruction pretty printer
pretty :: Integer -> String
pretty instr = 
  decode 32 instr (integer_instructions_dissasembly_list ++ cheri_instructions_dissasembly_list)

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

-- Power of two values clustered around 1.
geomBits :: Int -> Int -> Gen Integer
geomBits hi lo = frequency [(2^(32-i), return (2^i))| i <- [lo..(hi-1)]]

-- Generate memory offset
offset :: Gen Integer
offset = oneof [return 0, return 1, return 64, return 65]

genCHERI :: Gen Integer
genCHERI =
  frequency [
    (8,  encode cincoffsetimmediate (bits 12) src dest)
  ]
