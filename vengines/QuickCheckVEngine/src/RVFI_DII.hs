--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018, 2020 Alexandre Joannou
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

{-# LANGUAGE ScopedTypeVariables #-}

module RVFI_DII (
  module RVFI_DII.RVFI
, module RVFI_DII.DII
, readDIITrace
, readDIITraceFile
, readDIIDataFile
) where

import RVFI_DII.RVFI
import RVFI_DII.DII

import Data.List.Split
import System.IO
import Numeric
import Test.QuickCheck
import RISCV
import Template
import Templates.Utils
import InstrCodec

-- | Show instance for 'TestCase' now that 'diiInstruction' is in scope
instance Show TestCase where
  show testCase = show $ map diiInstruction (fromTestCase testCase)

-- | Turns a '[String]' representation of a DII trace into a 'TestCase'
readDIITrace :: [String] -> TestCase
readDIITrace inStr =
  let lns = map (head . (splitOn "#")) inStr            -- Remove comments
      trimmed = filter (not . null) lns                 -- Remove empty lines
      encInsts = map ((drop 2) .(!! 1) . words) trimmed -- Take only encoded instruction
      insts :: [Integer] = map (fst . head . readHex) encInsts
  in toTestCase insts

-- | Turns file representation of a DII trace into a 'TestCase'
readDIITraceFile :: FilePath -> IO TestCase
readDIITraceFile inFile = do
  handle <- openFile inFile ReadMode
  contents <- hGetContents handle
  return $ readDIITrace (lines contents)

-- | Turns a '[String]' representation of some data into a DII trace 'TestCase'
--   that initializes memory with that data
readDIIData :: [String] -> Gen TestCase
readDIIData ss = genTemplateUnsized $  (writeData addr ws)
                                    <> (li32 1 0x80000000)
                                    <> (Single $ InstrCodec.encode jalr 0 1 0)
  where (addr:ws) = map (fst . head . readHex . head . words) ss

-- | Turns a File representation of some data into a DII trace 'TestCase' that
--   initializes memory with that data
readDIIDataFile :: FilePath -> IO TestCase
readDIIDataFile inFile = do
  handle <- openFile inFile ReadMode
  contents <- hGetContents handle
  testCase <- generate $ readDIIData (lines contents)
  putStrLn $ show (testCaseInstCount testCase)
  return testCase
