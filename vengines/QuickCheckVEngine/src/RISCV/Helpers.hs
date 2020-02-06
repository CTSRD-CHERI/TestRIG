--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2019 Alexandre Joannou
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

{-# LANGUAGE BinaryLiterals #-}

module RISCV.Helpers (
  csrs_map
, csrs_indexFromName
, csrs_nameFromIndex
, reg
, int
, prettyR
, prettyI
, prettyL
, prettyS
, prettyU
, prettyB
, prettyF
, prettyR_2op
, prettyCSR
, prettyCSR_imm
, prettyR_A
, prettyR_A_1op
, fpRoundingMode
, prettyR_1op
, prettyR_1op_rm
, prettyR_rm
, prettyR4_rm
) where

import Data.Maybe (fromMaybe)
import Numeric (showHex)

---------------
-- CSRs helpers
---------------

csrs_indexFromName :: String -> Maybe Integer
csrs_indexFromName nm = lookup nm [ (b, a) | (a, b) <- csrs_map]

csrs_nameFromIndex :: Integer -> Maybe String
csrs_nameFromIndex idx = lookup idx csrs_map

csrs_map :: [(Integer, String)]
csrs_map = -- User Trap Setup
           [ (0x000, "ustatus")
           , (0x004, "uie")
           , (0x005, "utvec") ]
        ++ -- User Trap Handling
           [ (0x040, "uscratch")
           , (0x041, "uepc")
           , (0x042, "ucause")
           , (0x043, "utval")
           , (0x044, "uip") ]
        ++ -- User Floating-Point CSRs
           [ (0x001, "fflags")
           , (0x002, "frm")
           , (0x003, "fcsr") ]
        ++ -- User Counters/Timers
           [ (0xC00, "cycle")
           , (0xC01, "time")
           , (0xC02, "instret") ]
        ++ [ (0xC00 + x, "hpmcounter" ++ show x) | x <- [3..31] ]
        ++ [ (0xC80, "cycleh")
           , (0xC81, "timeh")
           , (0xC82, "instreth") ]
        ++ [ (0xC80 + x, "hpmcounter" ++ show x ++ "h") | x <- [3..31] ]
        ++ -- Supervisor Trap Setup
           [ (0x100, "sstatus")
           , (0x102, "sedeleg")
           , (0x103, "sideleg")
           , (0x104, "sie")
           , (0x105, "stvec")
           , (0x106, "scounteren") ]
        ++ -- Supervisor Trap Handling
           [ (0x140, "sscratch")
           , (0x141, "sepc")
           , (0x142, "scause")
           , (0x143, "stval")
           , (0x144, "sip") ]
        ++ -- Supervisor Protection and Translation
           [ (0x180, "satp") ]
        -- TODO Hypervisor CSRs
        ++ -- Machine Information Registers
           [ (0xF11, "mvendorid")
           , (0xF12, "marchid")
           , (0xF13, "mimpid")
           , (0xF14, "mhartid") ]
        ++ -- Machine Trap Setup
           [ (0x300, "mstatus")
           , (0x301, "misa")
           , (0x302, "medeleg")
           , (0x303, "mideleg")
           , (0x304, "mie")
           , (0x305, "mtvec")
           , (0x306, "mcounteren")
           , (0x310, "mstatush") ]
        ++ -- Machine Trap Handling
           [ (0x340, "mscratch")
           , (0x341, "mepc")
           , (0x342, "mcause")
           , (0x343, "mtval")
           , (0x344, "mip") ]
        ++ -- Machine Memory Protection
           [ (0x3A0 + x, "pmpcfg" ++ show x) | x <- [0..3] ]
        ++ [ (0x3B0 + x, "pmpaddr" ++ show x) | x <- [0..15] ]
        ++ -- Machine Counters/Timers
           [ (0xB00, "mcycle")
           , (0xB02, "minstret") ]
        ++ [ (0xB00 + x, "mhpmcounter" ++ show x) | x <- [3..31] ]
        ++ [ (0xB80, "mcycleh")
           , (0xB82, "minstreth") ]
        ++ [ (0xB80 + x, "mhpmcounter" ++ show x ++ "h") | x <- [3..31] ]
        ++ -- Machine Counter Setup
           [ (0x320, "mcountinhibit") ]
        ++ [ (0x320 + x, "mhpmevent" ++ show x) | x <- [3..31] ]
        -- TODO Debug/Trace Registers (shared with Debug Mode)
        -- TODO Debug Mode Registers
        -- List last checked from:
        -- The RISC-V Instruction Set Manual
        -- Volume II: Privileged Architecture
        -- Document Version 1.12-draft
        -- September 13, 2019

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

-- CSR instructions pretty printer
prettyCSR instr csr rs1 rd =
  concat [instr, " ", reg rd, ", ", csr_nm, ", ", reg rs1]
  where csr_nm  = (fromMaybe "unknown" (csrs_nameFromIndex csr)) ++ idx_str
        idx_str = " (0x" ++ showHex csr "" ++ ")"

prettyCSR_imm instr csr imm rd =
  concat [instr, " ", reg rd, ", ", csr_nm, ", ", int imm]
  where csr_nm  = (fromMaybe "unknown" (csrs_nameFromIndex csr)) ++ idx_str
        idx_str = " (0x" ++ showHex csr "" ++ ")"

-- R-type Atomic pretty printer
prettyR_A :: String -> Integer -> Integer -> Integer -> Integer -> Integer
          -> String
prettyR_A instr aq rl rs2 rs1 rd =
  concat $  [instr, " ", reg rd, ", ", reg rs1, ", ", reg rs2]
         ++ [if aq == 1 then " (aq)" else ""]
         ++ [if rl == 1 then " (rl)" else ""]

prettyR_A_1op :: String -> Integer -> Integer -> Integer -> Integer -> String
prettyR_A_1op instr aq rl rs1 rd =
  concat $  [instr, " ", reg rd, ", ", reg rs1]
         ++ [if aq == 1 then " (aq)" else ""]
         ++ [if rl == 1 then " (rl)" else ""]

-- Floating Point pretty printer
-- Floating Point rounding modes
fpRoundingMode :: Integer -> String
fpRoundingMode 0b000 = "rne"
fpRoundingMode 0b001 = "rtz"
fpRoundingMode 0b010 = "rdn"
fpRoundingMode 0b011 = "rup"
fpRoundingMode 0b100 = "rmm"
fpRoundingMode 0b101 = "Reserved5"
fpRoundingMode 0b110 = "Reserved6"
fpRoundingMode 0b111 = "rdyn"
fpRoundingMode x =
  "unsupported floating point rounding mode 0x" ++ (showHex x "")

prettyR_1op instr rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1]

prettyR_1op_rm instr rs1 rm rd =
  concat $  [instr, " ", reg rd, ", ", reg rs1]
         ++ [", " ++ fpRoundingMode rm ]

prettyR_rm instr rs2 rs1 rm rd =
  concat $  [instr, " ", reg rd, ", ", reg rs1, ", ", reg rs2]
         ++ [", " ++ fpRoundingMode rm ]

prettyR4_rm instr rs3 rs2 rs1 rm rd =
  concat $  [instr, " ", reg rd, ", ", reg rs1, ", ", reg rs2, ", ", reg rs3]
         ++ [", " ++ fpRoundingMode rm ]
