--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018 Alexandre Joannou
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
import Data.Bits
import qualified Data.Bits.Bitwise as BW
import Data.Binary
import Data.String
import Numeric (readHex, showHex, showIntAtBase)
import GHC.Generics (Generic)
import System.IO 
import Test.QuickCheck
import RISCV
import RVxxI
import Text.Printf
import Data.List.Split

rvfi_cmd_instruction = 1 :: Word8
rvfi_cmd_end = 0 :: Word8

data RVFI_DII_Instruction = RVFI_DII_Instruction {
  padding   :: Word8,
  rvfi_cmd  :: Word8,
  rvfi_time :: Word16,
  rvfi_ins_insn :: [Word32]
} deriving (Generic)
instance Binary RVFI_DII_Instruction
instance Num RVFI_DII_Instruction where
  fromInteger i =
    RVFI_DII_Instruction {
      padding   = 0,
      rvfi_cmd  = rvfi_cmd_instruction,
      rvfi_time = 1,
      rvfi_ins_insn = [fromInteger i]
    }
instance Arbitrary RVFI_DII_Instruction where
  arbitrary = do
    inst <- genArithmetic
    return RVFI_DII_Instruction {
      padding   = 0,
      rvfi_cmd  = rvfi_cmd_instruction,
      rvfi_time = 1,
      rvfi_ins_insn = map fromInteger inst
    }

rvfi_dii_gen :: Gen [Integer] -> Gen RVFI_DII_Instruction
rvfi_dii_gen instGenerator = do
  inst <- instGenerator
  return RVFI_DII_Instruction {
    padding   = 0,
    rvfi_cmd  = rvfi_cmd_instruction,
    rvfi_time = 1,
    rvfi_ins_insn = map fromInteger inst
  }

instance Show RVFI_DII_Instruction where
  show inst_tok = ".4byte 0x" ++ (showHex (head(rvfi_ins_insn inst_tok)) "")
               ++ " # " ++ pretty (toInteger (head(rvfi_ins_insn inst_tok)))
  showList inst_toks = showString (unlines (map show inst_toks))

read_rvfi_inst_trace :: [String] -> [RVFI_DII_Instruction]
read_rvfi_inst_trace inStr =
  let lns = map (head . (splitOn "#")) inStr in           -- Remove comments
  let trimmed = filter (not . null) lns in                -- Remove empty lines
  let insts = map ((drop 2) .(!! 1) . words) trimmed in   -- Take only encoded instruction
  map (fromInteger .fst . head . readHex) insts

read_rvfi_inst_trace_file :: FilePath -> IO [RVFI_DII_Instruction]
read_rvfi_inst_trace_file inFile = do
  handle <- openFile inFile ReadMode
  contents <- hGetContents handle
  return $ read_rvfi_inst_trace (lines contents)

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

maskUpper :: Word64 -> Word64
maskUpper x = (x Data.Bits..&. 0x00000000FFFFFFFF)

maskWith :: Word64 -> Word8 -> Word64
maskWith a b = a Data.Bits..&. mask
               where mask = BW.fromListLE $ concatMap (reverse.dropWhile(not).reverse.((take 8).repeat)) (BW.toListLE b)

instance Eq RVFI_DII_Execution where
  x == y
    | rvfi_halt x /= 0 = (rvfi_halt x) == (rvfi_halt y)
    | rvfi_trap x /= 0 = ((rvfi_trap x) == (rvfi_trap y)) && (maskUpper (rvfi_pc_wdata x)) == (maskUpper (rvfi_pc_wdata y))
    | otherwise = (maskUpper (rvfi_exe_insn x)) == (maskUpper (rvfi_exe_insn y)) &&
                  (maskUpper (rvfi_rd_wdata x)) == (maskUpper (rvfi_rd_wdata y)) &&
                  (rvfi_mem_wmask x) == (rvfi_mem_wmask y) &&
                  ((rvfi_mem_wmask x == 0) || ((maskUpper (rvfi_mem_addr x)) == (maskUpper (rvfi_mem_addr y)))) &&
                  (maskUpper (rvfi_pc_wdata x)) == (maskUpper (rvfi_pc_wdata y)) &&
                  (maskWith (rvfi_mem_wdata x) (rvfi_mem_wmask x)) == (maskWith (rvfi_mem_wdata y) (rvfi_mem_wmask y))

instance Show RVFI_DII_Execution where
  show tok
    | rvfi_halt tok /= 0 = "halt token"
    | otherwise = printf "Trap: %5s, PCWD: 0x%016x, RWD: 0x%016x, MA: 0x%016x, MWD: 0x%016x, MWM: 0b%08b, I: 0x%016x (%s)"
                  (show ((rvfi_trap tok) /= 0)) -- Trap
                  (rvfi_pc_wdata tok)           -- PCWD
                  (rvfi_rd_wdata tok)           -- RWD
                  (rvfi_mem_addr tok)           -- MA
                  (rvfi_mem_wdata tok)          -- MWD
                  (rvfi_mem_wmask tok)          -- MWM
                  (rvfi_exe_insn tok) (pretty (toInteger (rvfi_exe_insn tok))) -- Inst
