--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2018 Jonathan Woodruff
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

{-# LANGUAGE DeriveGeneric #-}
module RVFI_DII where

import Data.Word
import Data.Binary
import Numeric (showHex, showIntAtBase)
import GHC.Generics (Generic)
import Test.QuickCheck
import RISCV

rvfi_cmd_instruction = 1 :: Word8
rvfi_cmd_end = 0 :: Word8

data RVFI_DII_Instruction = RVFI_DII_Instruction {
  padding   :: Word8,
  rvfi_cmd  :: Word8,
  rvfi_time :: Word16,
  rvfi_ins_insn :: Word32
} deriving (Generic)
instance Binary RVFI_DII_Instruction
instance Arbitrary RVFI_DII_Instruction where
  arbitrary = do
    inst <- genArithmetic
    return RVFI_DII_Instruction {
      padding   = 0,
      rvfi_cmd  = rvfi_cmd_instruction,
      rvfi_time = 1,
      rvfi_ins_insn = fromInteger inst
    }

rvfi_dii_gen :: Gen Integer -> Gen RVFI_DII_Instruction
rvfi_dii_gen instGenerator = do
  inst <- instGenerator
  return RVFI_DII_Instruction {
    padding   = 0,
    rvfi_cmd  = rvfi_cmd_instruction,
    rvfi_time = 1,
    rvfi_ins_insn = (fromInteger inst)
  }

instance Show RVFI_DII_Instruction where
  show inst_tok = ".4byte 0x" ++ (showHex (rvfi_ins_insn inst_tok) "")
               ++ " # " ++ pretty (toInteger (rvfi_ins_insn inst_tok))
  showList inst_toks = showString (unlines (map show inst_toks))

data RVFI_DII_Execution = RVFI_DII_Execution {
  rvfi_intr :: Word8,
  rvfi_halt :: Word8,
  rvfi_trap :: Word8,
  rvfi_rd_addr :: Word8,
  rvfi_rs2_addr :: Word8,
  rvfi_rs1_addr :: Word8,
  rvfi_mem_wmask :: Word8,
  rvfi_mem_rmask :: Word8,
  rvfi_mem_wdata :: Word64,
  rvfi_mem_rdata :: Word64,
  rvfi_mem_addr :: Word64,
  rvfi_rd_wdata :: Word64,
  rvfi_rs2_data :: Word64,
  rvfi_rs1_data :: Word64,
  rvfi_exe_insn :: Word64,
  rvfi_pc_wdata :: Word64,
  rvfi_pc_rdata :: Word64,
  rvfi_order :: Word64
} deriving (Generic)
instance Binary RVFI_DII_Execution

instance Eq RVFI_DII_Execution where
  x == y =
    (rvfi_exe_insn x) == (rvfi_exe_insn y) &&
    (rvfi_rd_wdata x) == (rvfi_rd_wdata y) &&
    (rvfi_mem_addr x) == (rvfi_mem_addr y) &&
    (rvfi_pc_wdata x) == (rvfi_pc_wdata y) &&
    (rvfi_mem_wdata x) == (rvfi_mem_wdata y)

instance Show RVFI_DII_Execution where
  show tok = "  PCWD:0x" ++ (showHex (rvfi_pc_wdata tok) "") ++
             "  RWD:0x" ++ (showHex (rvfi_rd_wdata tok) "") ++
             "  MA:0x" ++ (showHex (rvfi_mem_addr tok) "") ++
             "  MWD:0x" ++ (showHex (rvfi_mem_wdata tok) "") ++
             "  I:0x" ++ (showHex (rvfi_exe_insn tok) "") ++ " " ++ pretty (toInteger (rvfi_exe_insn tok)) ++ "\n"