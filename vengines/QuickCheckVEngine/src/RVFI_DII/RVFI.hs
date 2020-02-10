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

module RVFI_DII.RVFI (
  RVFI_Packet
, rvfiIsHalt
, rvfiIsTrap
, rvfiCheck
, rvfiShowCheck
) where

import Data.Word
import Data.Bits
import qualified Data.Bits.Bitwise as BW
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import RISCV
import Text.Printf

-- * Type synonyms

-- | Type synonym for a RISCV register index
type RV_RegIdx = Word8

-- | Type synonym for a RISCV XLEN-bit word
type RV_WordXLEN = Word64

-- | Type synonym for a RISCV XLEN-bit word
type RV_WordXLEN_ByteMask = Word8

-- * Definition of the RISC-V Formal Interface

-- | The 'RVFI_Packet' type captures (a subset of) the RISC-V Formal Interface
--   as defined at
--   https://github.com/SymbioticEDA/riscv-formal/blob/master/docs/rvfi.md
data RVFI_Packet = RVFI_Packet {
-- * Metadata
  rvfi_valid :: Word8  -- ^ TODO
, rvfi_order :: Word64 -- ^ TODO
, rvfi_insn  :: Word64 -- ^ TODO
, rvfi_trap  :: Word8  -- ^ TODO
, rvfi_halt  :: Word8  -- ^ TODO
, rvfi_intr  :: Word8  -- ^ TODO
--, rvfi_mode  :: RV_PrivMode -- ^ TODO
--, rvfi_ixl   :: RV_XL       -- ^ TODO
-- * Integer Register Read/Write
, rvfi_rs1_addr  :: RV_RegIdx   -- ^ TODO
, rvfi_rs2_addr  :: RV_RegIdx   -- ^ TODO
, rvfi_rs1_rdata :: RV_WordXLEN -- ^ TODO
, rvfi_rs2_rdata :: RV_WordXLEN -- ^ TODO
, rvfi_rd_addr   :: RV_RegIdx   -- ^ TODO
, rvfi_rd_wdata  :: RV_WordXLEN -- ^ TODO
-- * Program Counter
, rvfi_pc_rdata :: RV_WordXLEN -- ^ TODO
, rvfi_pc_wdata :: RV_WordXLEN -- ^ TODO
-- * Memory Access
, rvfi_mem_addr  :: RV_WordXLEN          -- ^ TODO
, rvfi_mem_rmask :: RV_WordXLEN_ByteMask -- ^ TODO
, rvfi_mem_wmask :: RV_WordXLEN_ByteMask -- ^ TODO
, rvfi_mem_rdata :: RV_WordXLEN          -- ^ TODO
, rvfi_mem_wdata :: RV_WordXLEN          -- ^ TODO
-- Further TODOs in the RVFI specification:
-- Control and Status Registers (CSRs)
-- Modelling of Floating-Point State
-- Handling of Speculative Execution
-- Modelling of Virtual Memory
-- Modelling of Atomic Memory Operations
-- Skipping instructions
}

instance Binary RVFI_Packet where
  put pkt =    putWord8    (rvfi_intr pkt)
            <> putWord8    (rvfi_halt pkt)
            <> putWord8    (rvfi_trap pkt)
            <> putWord8    (rvfi_rd_addr pkt)
            <> putWord8    (rvfi_rs2_addr pkt)
            <> putWord8    (rvfi_rs1_addr pkt)
            <> putWord8    (rvfi_mem_wmask pkt)
            <> putWord8    (rvfi_mem_rmask pkt)
            <> putWord64be (rvfi_mem_wdata pkt)
            <> putWord64be (rvfi_mem_rdata pkt)
            <> putWord64be (rvfi_mem_addr pkt)
            <> putWord64be (rvfi_rd_wdata pkt)
            <> putWord64be (rvfi_rs2_rdata pkt)
            <> putWord64be (rvfi_rs1_rdata pkt)
            <> putWord64be (rvfi_insn pkt)
            <> putWord64be (rvfi_pc_wdata pkt)
            <> putWord64be (rvfi_pc_rdata pkt)
            <> putWord64be (rvfi_order pkt)
  get = do intr      <- getWord8
           halt      <- getWord8
           trap      <- getWord8
           rd_addr   <- getWord8
           rs2_addr  <- getWord8
           rs1_addr  <- getWord8
           mem_wmask <- getWord8
           mem_rmask <- getWord8
           mem_wdata <- getWord64be
           mem_rdata <- getWord64be
           mem_addr  <- getWord64be
           rd_wdata  <- getWord64be
           rs2_rdata <- getWord64be
           rs1_rdata <- getWord64be
           insn      <- getWord64be
           pc_wdata  <- getWord64be
           pc_rdata  <- getWord64be
           order     <- getWord64be
           return $ RVFI_Packet {
               rvfi_valid = 1
             , rvfi_order = order
             , rvfi_insn  = insn
             , rvfi_trap  = trap
             , rvfi_halt  = halt
             , rvfi_intr  = intr
             --, rvfi_mode = _
             --, rvfi_ixl  = _
             , rvfi_rs1_addr  = rs1_addr
             , rvfi_rs2_addr  = rs2_addr
             , rvfi_rs1_rdata = rs1_rdata
             , rvfi_rs2_rdata = rs2_rdata
             , rvfi_rd_addr   = rd_addr
             , rvfi_rd_wdata  = rd_wdata
             -- * Program Counter
             , rvfi_pc_rdata = pc_rdata
             , rvfi_pc_wdata = pc_wdata
             -- * Memory Access
             , rvfi_mem_addr  = mem_addr
             , rvfi_mem_rmask = mem_rmask
             , rvfi_mem_wmask = mem_wmask
             , rvfi_mem_rdata = mem_rdata
             , rvfi_mem_wdata = mem_wdata
             -- Further TODOs in the RVFI specification ...
             }

instance Show RVFI_Packet where
  show tok
    | rvfiIsHalt tok = "halt token"
    | otherwise = printf "Trap: %5s, PCWD: 0x%016x, RD: %02d, RWD: 0x%016x, MA: 0x%016x, MWD: 0x%016x, MWM: 0b%08b, I: 0x%016x (%s)"
                  (show $ rvfi_trap tok) -- Trap
                  (rvfi_pc_wdata tok)    -- PCWD
                  (rvfi_rd_addr tok)     -- RD
                  (rvfi_rd_wdata tok)    -- RWD
                  (rvfi_mem_addr tok)    -- MA
                  (rvfi_mem_wdata tok)   -- MWD
                  (rvfi_mem_wmask tok)   -- MWM
                  (rvfi_insn tok) (pretty (toInteger (rvfi_insn tok))) -- Inst

-- * 'RVFI_Packet' queries

-- | Return 'True' for halt 'RVFI_Packet's
rvfiIsHalt :: RVFI_Packet -> Bool
rvfiIsHalt x = rvfi_halt x /= 0

-- | Return 'True' for trap 'RVFI_Packet's
rvfiIsTrap :: RVFI_Packet -> Bool
rvfiIsTrap x = rvfi_trap x /= 0

-- * 'RVFI_Packet' checks and displays

-- | Compare 'RVFI_Packet's
rvfiCheck :: Bool -> RVFI_Packet -> RVFI_Packet -> Bool
rvfiCheck is64 x y
  | rvfiIsHalt x = (rvfi_halt x) == (rvfi_halt y)
  | rvfiIsTrap x = ((rvfi_trap x) == (rvfi_trap y)) && (maskUpper is64 (rvfi_pc_wdata x)) == (maskUpper is64 (rvfi_pc_wdata y))
  | otherwise = (maskUpper False $ rvfi_insn x) == (maskUpper False $ rvfi_insn y) &&
                (rvfi_trap x) == (rvfi_trap y) && (rvfi_halt x) == (rvfi_halt y) &&
                (rvfi_rd_addr x == rvfi_rd_addr y) &&
                ((rvfi_rd_addr x == 0) || (maskUpper is64 (rvfi_rd_wdata x) == maskUpper is64 (rvfi_rd_wdata y))) &&
                (rvfi_mem_wmask x) == (rvfi_mem_wmask y) &&
                ((rvfi_mem_wmask x == 0) || ((maskUpper is64 (rvfi_mem_addr x)) == (maskUpper is64 (rvfi_mem_addr y)))) &&
                (maskUpper is64 (rvfi_pc_wdata x)) == (maskUpper is64 (rvfi_pc_wdata y)) &&
                (maskWith (rvfi_mem_wdata x) (rvfi_mem_wmask x)) == (maskWith (rvfi_mem_wdata y) (rvfi_mem_wmask y))
  where maskUpper is64 x = if is64 then x else (x Data.Bits..&. 0x00000000FFFFFFFF)
        byteMask2bitMask mask = BW.fromListLE $ concatMap ((take 8).repeat) (BW.toListLE mask)
        maskWith a b = a Data.Bits..&. byteMask2bitMask b

-- | Compare 2 'RVFI_Packet's and produce a 'String' output displaying the
--   the content of the packet once only for equal inputs or the content of
--   each input 'RVFI_Packet' if inputs are not succeeding the 'rvfiCheck'
rvfiShowCheck :: Bool -> RVFI_Packet -> RVFI_Packet -> String
rvfiShowCheck is64 x y
  | rvfiCheck is64 x y = "     " ++ show x
  | otherwise          = " A < " ++ show x ++ "\n B > " ++ show y
