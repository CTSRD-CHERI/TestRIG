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
import Data.Char
import Text.Printf
import Text.Regex.Posix
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Environment
import System.Directory
import System.Console.GetOpt
import System.IO.Unsafe
import Data.Maybe ( isJust, fromMaybe )
import Data.List
import System.FilePath.Windows
import System.Exit
import System.IO
import RVFI_DII
import RISCV
import CHERI
import RVxxI

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
    , arch          :: String
    , instrPort     :: Maybe String
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
    , arch          = "32i"
    , instrPort     = Nothing
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
  , Option ['r']     ["architecture"]
      (ReqArg (\ f opts -> opts { arch = map toLower f }) "ARCHITECTURE")
        "Specify ARCHITECTURE to be verified (e.g. 32i)"
  , Option ['i']     ["instruction generator port"]
      (ReqArg (\ f opts -> opts { instrPort = Just f }) "PORT")
        "Connect to an external instruction generator on PORT"
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
  addrA <- resolve (impAIP flags) (impAPort flags)
  addrB <- resolve (impBIP flags) (impBPort flags)
  socA <- open addrA
  socB <- open addrB
  sendInstructionTrace socA ([RVFI_DII_Instruction {
                                            padding   = 0,
                                            rvfi_cmd  = rvfi_cmd_end,
                                            rvfi_time = 1,
                                            rvfi_ins_insn = [0]
                                          }])
  _ <- receiveExecutionTrace False socA
  sendInstructionTrace socB ([RVFI_DII_Instruction {
                                            padding   = 0,
                                            rvfi_cmd  = rvfi_cmd_end,
                                            rvfi_time = 1,
                                            rvfi_ins_insn = [0]
                                          }])
  _ <- receiveExecutionTrace False socB
  addrInstr <- mapM (resolve "127.0.0.1") (instrPort flags)
  instrSoc <- mapM open addrInstr
  --
  let checkResult = if (optVerbose flags)
                    then verboseCheckWithResult
                    else quickCheckWithResult
  let checkSingle trace = do
      quickCheckWith (Args Nothing 1 1 2048 True 0) (prop (return trace) socA socB True)
  let checkGen gen = do
      result <- checkResult (Args Nothing (nTests flags) 1 2048 True 1000) (prop (listOf (rvfi_dii_gen gen)) socA socB (optVerbose flags))
      case result of
        Failure {} -> do
          writeFile "last_failure.S" ("# last failing test case:\n" ++ (unlines (failingTestCase result)))
          putStrLn "Replaying shrunk failed test case:"
          checkSingle (read_rvfi_inst_trace (failingTestCase result))
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
      putStrLn $ "Reading trace from " ++ fileName ++ ":"
      trace <- read_rvfi_inst_trace_file fileName
      checkSingle trace
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
          case instrSoc of
            Nothing -> do
              when (((arch flags) =~ ("i"::String)) ::Bool) (
                do
                  putStrLn "rv32i Arithmetic Verification:"
                  checkGen genArithmetic
                  putStrLn "rv32i Memory Verification:"
                  checkGen genMemory
                  putStrLn "rv32i Control Flow Verification:"
                  checkGen genControlFlow
                  putStrLn "rv32i All Verification:"
                  checkGen genAll
                  )
              when (((arch flags) =~ ("xcheri"::String)) ::Bool) (
                do
                  putStrLn "xCHERI Capability Inspection Verification:"
                  checkGen genCHERIinspection
                  putStrLn "xCHERI Capability Arithmetic Verification:"
                  checkGen genCHERIarithmetic
                  putStrLn "xCHERI Capability Miscellaneous Verification:"
                  checkGen genCHERImisc
                  putStrLn "xCHERI Capability Control Flow Verification:"
                  checkGen genCHERIcontrol
                  )
            Just sock -> do
              checkGen (genInstrServer sock)
  --
  close socA
  close socB
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
prop gen socA socB doLog = forAllShrink gen shrink ( \instTrace -> monadicIO ( run ( do
  let instTraceTerminated = (instTrace ++ [RVFI_DII_Instruction {
                                            padding   = 0,
                                            rvfi_cmd  = rvfi_cmd_end,
                                            rvfi_time = 1,
                                            rvfi_ins_insn = 0
                                          }])
  sendInstructionTrace socA instTraceTerminated
  sendInstructionTrace socB instTraceTerminated

  when doLog $ putStrLn "----------------------------------------------------------------------"
  when doLog $ putStrLn "Socket A Trace (model)"
  when doLog $ putStrLn "----------------------"
  modTrace <- receiveExecutionTrace doLog socA
  when doLog $ putStrLn "Socket B Trace (implementation)"
  when doLog $ putStrLn "-------------------------------"
  impTrace <- receiveExecutionTrace doLog socB

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
receiveExecutionTrace :: Bool -> Socket -> IO ([RVFI_DII_Execution])
receiveExecutionTrace doLog sock = do
  msg <- recv sock 88
  let traceEntry = (decode (BS.reverse msg)) :: RVFI_DII_Execution
  when doLog $ putStrLn ("\t"++(show traceEntry))
  if ((rvfi_halt traceEntry) == 1)
    then return [traceEntry]
    else do
      remainderOfTrace <- receiveExecutionTrace doLog sock
      return (traceEntry:remainderOfTrace)

--------------------------------------------------------------------------------

genInstrServer :: Socket -> Gen Integer
genInstrServer sock = do
  seed :: Int32 <- arbitraryBoundedRandom
  -- This should be safe so long as the server returns the same instruction when
  -- given the same seed.
  let msg = unsafePerformIO (
        do
          sendAll sock (encode seed)
          recv sock 4)
  return (toInteger (decode (BS.reverse msg) :: Int32))
