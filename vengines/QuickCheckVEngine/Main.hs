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

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception as E
-- import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BS
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy --(recv, sendAll)
import Data.Int
import Data.Binary
import Text.Printf
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import RVFI_DII

main :: IO ()
main = withSocketsDo $ do
    addrMod <- resolve "127.0.0.1" "5000"
    addrImp <- resolve "127.0.0.1" "5001"
    modSoc <- open addrMod
    impSoc <- open addrImp
    --instTrace <- sequence (take 8 (repeat generateInstructionTraceEntry))
    quickCheck (withMaxSuccess 100 (prop modSoc impSoc))
    --success <- prop modSoc impSoc (instTrace ++ [RVFI_DII_Instruction {
    --  padding   = 0,
    --  rvfi_cmd  = rvfi_cmd_end,
    --  rvfi_time = 1,
    --  rvfi_ins_insn = 0
    --}])
    --putStr (show success)
    close modSoc
    close impSoc
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        return sock

prop :: Socket -> Socket -> [RVFI_DII_Instruction] -> Property
prop modSoc impSoc instTrace = monadicIO $ run $ do
  let instTraceTerminated = (instTrace ++ [RVFI_DII_Instruction {
                                            padding   = 0,
                                            rvfi_cmd  = rvfi_cmd_end,
                                            rvfi_time = 1,
                                            rvfi_ins_insn = 0
                                          }])
  sendInstructionTrace modSoc instTraceTerminated
  sendInstructionTrace impSoc instTraceTerminated
  --putStr(" receive the model ")
  modTrace <- receiveExecutionTrace modSoc
  --putStr(" and now implementation ")
  impTrace <- receiveExecutionTrace impSoc
  return (and (zipWith compareExecutionTraceEntry modTrace impTrace))
  
  
-- Send an instruction trace
sendInstructionTrace :: Socket -> [RVFI_DII_Instruction] -> IO ()
sendInstructionTrace sock instTrace =
  mapM_ (sendInstruction sock) instTrace
  
-- Send a single instruction
sendInstruction :: Socket -> RVFI_DII_Instruction -> IO ()
sendInstruction sock inst = do
  sendAll sock (BS.reverse (encode inst))
  return ()
  

-- Receive an execution trace
receiveExecutionTrace :: Socket -> IO ([RVFI_DII_Execution])
receiveExecutionTrace sock = do
  msg <- recv sock 88
  let traceEntry = (decode (BS.reverse msg)) :: RVFI_DII_Execution
  -- print traceEntry
  if ((rvfi_halt traceEntry) == 1)
    then return [traceEntry]
    else do
      remainderOfTrace <- receiveExecutionTrace sock
      return (traceEntry:remainderOfTrace)

-- Compare two execution trace entries
compareExecutionTraceEntry :: RVFI_DII_Execution -> RVFI_DII_Execution -> Bool
compareExecutionTraceEntry modEntry impEntry =
  (rvfi_exe_insn modEntry) == (rvfi_exe_insn impEntry) &&
  (rvfi_rd_wdata modEntry) == (rvfi_rd_wdata impEntry)
