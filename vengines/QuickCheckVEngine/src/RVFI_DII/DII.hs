--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018-2020 Alexandre Joannou
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

module RVFI_DII.DII (
  DII_Packet     -- The 'DII_Packet' type used to send a DII command
, diiInstruction -- ^ Construct a instruction 'DII_Packet'
, diiEnd         -- ^ Construct an end 'DII_Packet'
) where

import Data.Word
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import RISCV
import Text.Printf
import Data.List.Split
import Template

-- * Local types and command Helpers

-- | Type synonym for a DII command
type DII_Cmd = Word8
-- | Type synonym for a DII timestamp
type DII_Time = Word16
-- | Type synonym for a DII instruction
type DII_Instruction = Word32

-- | A DII instruction command
dii_cmd_instruction :: DII_Cmd
dii_cmd_instruction = 1
-- | A DII end command
dii_cmd_end :: DII_Cmd
dii_cmd_end = 0

-- * Definition of the DII interface

-- | The 'DII_Packet' type captures the DII interface as defined in
--   https://github.com/CTSRD-CHERI/TestRIG/blob/master/RVFI-DII.md
data DII_Packet = DII_Packet { dii_cmd  :: DII_Cmd
                             , dii_time :: DII_Time
                             , dii_insn :: DII_Instruction }
-- | A 'DII_Packet' is serialized to a 32-bit word with 8 bits of padding at
--   the front
instance Binary DII_Packet where
  put pkt =    putWord8 0
            <> putWord8 (dii_cmd pkt)
            <> putWord16be (dii_time pkt)
            <> putWord32be (dii_insn pkt)
  get = do getWord8
           cmd  <- getWord8
           time <- getWord16be
           insn <- getWord32be
           return $ DII_Packet { dii_cmd  = cmd
                               , dii_time = time
                               , dii_insn = insn }
instance Show DII_Packet where
  show inst_tok = printf ".4byte 0x%08x # %s"
                         (dii_insn inst_tok)
                         (pretty $ toInteger $ dii_insn inst_tok)
  showList inst_toks = showString (unlines (fmap show inst_toks))
instance Num DII_Packet where
  fromInteger i = DII_Packet { dii_cmd  = dii_cmd_instruction
                             , dii_time = 1
                             , dii_insn = fromInteger i }
  (+)    = error "(+) is not defined on DII_Packet"
  (*)    = error "(*) is not defined on DII_Packet"
  abs    = error "abs is not defined on DII_Packet"
  signum = error "signum is not defined on DII_Packet"
  negate = error "negate is not defined on DII_Packet"

-- * Helper functions to define 'DII_Packet's

-- | Construct a instruction 'DII_Packet'
diiInstruction :: Integer -> DII_Packet
diiInstruction inst = DII_Packet { dii_cmd  = dii_cmd_instruction
                                 , dii_time = 1
                                 , dii_insn = fromInteger inst }

-- | Construct an end 'DII_Packet'
diiEnd :: DII_Packet
diiEnd = DII_Packet { dii_cmd  = dii_cmd_end
                    , dii_time = 1
                    , dii_insn = 0 }
