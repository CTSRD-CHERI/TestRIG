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

import Data.Int
import Data.Binary
import GHC.Generics (Generic)
import Test.QuickCheck
import RISCV

rvfi_cmd_instruction = 1 :: Int8
rvfi_cmd_end = 0 :: Int8

data RVFI_DII_Instruction = RVFI_DII_Instruction {
  padding   :: Int8,
  rvfi_cmd  :: Int8,
  rvfi_time :: Int16,
  rvfi_ins_insn :: Int32
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
instance Show RVFI_DII_Instruction where
  show inst_tok = pretty (toInteger (rvfi_ins_insn inst_tok))

data RVFI_DII_Execution = RVFI_DII_Execution {
  rvfi_intr :: Int8,
  rvfi_halt :: Int8,
  rvfi_trap :: Int8,
  rvfi_rd_addr :: Int8,
  rvfi_rs2_addr :: Int8,
  rvfi_rs1_addr :: Int8,
  rvfi_mem_wmask :: Int8,
  rvfi_mem_rmask :: Int8,
  rvfi_mem_wdata :: Int64,
  rvfi_mem_rdata :: Int64,
  rvfi_mem_addr :: Int64,
  rvfi_rd_wdata :: Int64,
  rvfi_rs2_data :: Int64,
  rvfi_rs1_data :: Int64,
  rvfi_exe_insn :: Int64,
  rvfi_pc_wdata :: Int64,
  rvfi_pc_rdata :: Int64,
  rvfi_order :: Int64
} deriving (Show, Generic)
instance Binary RVFI_DII_Execution
