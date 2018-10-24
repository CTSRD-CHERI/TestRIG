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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import System.Environment
import System.Directory
import System.Console.GetOpt
import Data.Maybe ( isJust, fromMaybe )
import Data.List
import System.FilePath.Windows
import System.Exit
import System.IO
import RVFI_DII
import RISCV

-- command line arguments
--------------------------------------------------------------------------------
data Options = Options
    { optVerbose    :: Bool
    , nTests        :: Int
    , impAPort      :: String
    , impAIP        :: String
    , impBPort      :: String
    , impBIP        :: String
    , instTraceFile :: Maybe FilePath
    , instDirectory :: Maybe FilePath
    } deriving Show

defaultOptions = Options
    { optVerbose    = False
    , nTests        = 100
    , impAPort      = "5000"
    , impAIP        = "127.0.0.1"
    , impBPort      = "5001"
    , impBIP        = "127.0.0.1"
    , instTraceFile = Nothing
    , instDirectory = Nothing
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v']     ["verbose"]
      (NoArg (\ opts -> opts { optVerbose = True }))
        "Turn on verbose output"
  , Option ['n']     ["number-of-tests"]
      (ReqArg (\ f opts -> opts { nTests = read f }) "NUMTESTS")
        "Specify NUMTESTS the sumber of tests to run"
  , Option ['a']     ["implementation-A-port"]
      (ReqArg (\ f opts -> opts { impAPort = f }) "PORT")
        "Specify which PORT to use for implementation A"
  , Option ['A']     ["implementaton-A-ip"]
      (ReqArg (\ f opts -> opts { impAIP = f }) "IP")
        "Specify which IP to use for implementation A"
  , Option ['b']     ["implementation-B-port"]
      (ReqArg (\ f opts -> opts { impBPort = f }) "PORT")
        "Specify which PORT to use for implementation B"
  , Option ['B']     ["implementaton-B-ip"]
      (ReqArg (\ f opts -> opts { impBIP = f }) "IP")
        "Specify which IP to use for implementation B"
  , Option ['t']     ["trace-file"]
      (ReqArg (\ f opts -> opts { instTraceFile = Just f }) "PATH")
        "Specify PATH a trace file to use as the instruction trace to replay"
  , Option ['d']     ["trace-directory"]
      (ReqArg (\ f opts -> opts { instDirectory = Just f }) "PATH")
        "Specify PATH a directory which contains trace files to replay"
  ]

commandOpts :: [String] -> IO (Options, [String])
commandOpts argv =
  case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: QCTestRIG [OPTION...] files..."

--------------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
  -- parse command line arguments
  rawArgs <- getArgs
  (flags, leftover) <- commandOpts rawArgs
  when (optVerbose flags) $ print flags
  -- initialize model and implementation sockets
  addrMod <- resolve (impAIP flags) (impAPort flags)
  addrImp <- resolve (impBIP flags) (impBPort flags)
  modSoc <- open addrMod
  impSoc <- open addrImp
  --
  let checkResult = if (optVerbose flags)
                    then verboseCheckResult
                    else quickCheckResult
  let check = if (optVerbose flags)
              then verboseCheck
              else quickCheck
  let checkGen gen = do
      result <- checkResult (withMaxSuccess (nTests flags) (prop (listOf (rvfi_dii_gen gen)) modSoc impSoc (optVerbose flags)))
      case result of
        Failure {} -> do
          --writeFile "last_failure.S" ("# last failing test case:\n" ++ (failingTestCase result))
          writeFile "last_failure.S" ("# last failing test case:\n" ++ (unlines (failingTestCase result)))
          putStrLn "Save this trace? (y?)"
          ans <- getLine
          when (ans == "y" || ans == "Y") $ do
            putStrLn "What <fileName>.S?"
            fileName <- getLine
            putStrLn "One-line description?"
            comment <- getLine
            writeFile (fileName ++ ".S") ("# " ++ comment
                                          ++ "\n" ++ (unlines (failingTestCase result)))
          return ()
        other -> return ()
  let checkFile (fileName :: FilePath) = do
      print ("Reading trace from " ++ fileName ++ ":")
      trace <- read_rvfi_inst_trace fileName
      check (withMaxSuccess 1 (prop (return trace) modSoc impSoc (optVerbose flags)))
  --
  case (instTraceFile flags) of
    Just fileName -> do
      checkFile fileName
    Nothing -> do
      case (instDirectory flags) of
        Just directory -> do
          fileNames <- getDirectoryContents directory
          let culledFileNames = filter (\x -> (takeExtension x) == ".S") fileNames
          let fullCulledFileNames = map (\x -> directory ++ "/" ++ x) culledFileNames
          mapM_ checkFile fullCulledFileNames
        Nothing -> do
          print "RV32I Arithmetic Verification:"
          checkGen genArithmetic
          print "RV32I Memory Verification:"
          checkGen genMemory
          print "RV32I Control Flow Verification:"
          checkGen genControlFlow
          print "RV32I All Verification:"
          checkGen genAll
  --
  close modSoc
  close impSoc
  --
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        return sock

--------------------------------------------------------------------------------
        
prop :: Gen [RVFI_DII_Instruction] -> Socket -> Socket -> Bool -> Property
prop gen modSoc impSoc doLog = forAllShrink gen shrink ( \instTrace -> monadicIO ( run ( do
  let instTraceTerminated = (instTrace ++ [RVFI_DII_Instruction {
                                            padding   = 0,
                                            rvfi_cmd  = rvfi_cmd_end,
                                            rvfi_time = 1,
                                            rvfi_ins_insn = 0
                                          }])
  sendInstructionTrace modSoc instTraceTerminated
  sendInstructionTrace impSoc instTraceTerminated

  modTrace <- receiveExecutionTrace modSoc
  impTrace <- receiveExecutionTrace impSoc
  when doLog $ do
    print " model          Trace "
    print modTrace
    print " implementation Trace "
    print impTrace
  return (and (zipWith (==) modTrace impTrace)))))

--------------------------------------------------------------------------------

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
  --print traceEntry
  if ((rvfi_halt traceEntry) == 1)
    then return [traceEntry]
    else do
      remainderOfTrace <- receiveExecutionTrace sock
      return (traceEntry:remainderOfTrace)
