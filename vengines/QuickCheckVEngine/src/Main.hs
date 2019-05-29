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
import Data.Maybe ( isJust, isNothing, fromMaybe )
import Data.List
import Data.List.Split
import System.FilePath.Windows
import System.Exit
import System.IO
import RVFI_DII
import RISCV
import CHERI
import RVxxI
import Template
import System.Timeout

import GenAll
import GenArithmetic
import GenMemory
import RandomTest
import GenControlFlow
import MemUtils
import GenCHERI

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
    , saveDir       :: Maybe FilePath
    , timeoutDelay  :: Int
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
    , saveDir       = Nothing
    , timeoutDelay  = 2000000 -- 2 seconds
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v']     ["verbose"]
      (NoArg (\ opts -> opts { optVerbose = True }))
        "Turn on verbose output"
  , Option ['n']     ["number-of-tests"]
      (ReqArg (\ f opts -> opts { nTests = read f }) "NUMTESTS")
        "Specify NUMTESTS the number of tests to run"
  , Option ['a']     ["implementation-A-port"]
      (ReqArg (\ f opts -> opts { impAPort = f }) "PORT")
        "Specify which PORT to use for implementation A"
  , Option ['A']     ["implementation-A-ip"]
      (ReqArg (\ f opts -> opts { impAIP = f }) "IP")
        "Specify which IP to use for implementation A"
  , Option ['b']     ["implementation-B-port"]
      (ReqArg (\ f opts -> opts { impBPort = f }) "PORT")
        "Specify which PORT to use for implementation B"
  , Option ['B']     ["implementation-B-ip"]
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
  , Option ['s']     ["save-dir"]
      (ReqArg (\ f opts -> opts { saveDir = Just f }) "PATH")
        "Keep running, saving any new failures to files"
  , Option ['T']     ["timeout"]
      (ReqArg (\ f opts -> opts { timeoutDelay = read f }) "TIMEOUT")
        "Timeout after TIMEOUT microseconds of A or B not responding"
  ]

commandOpts :: [String] -> IO (Options, [String])
commandOpts argv =
  case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: QCVEngine [OPTION...] files..."

--------------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
  -- parse command line arguments
  rawArgs <- getArgs
  (flags, leftover) <- commandOpts rawArgs
  when (optVerbose flags) $ print flags
  let archStrings = splitOn "x" (arch flags)
  -- initialize model and implementation sockets
  addrA <- resolve (impAIP flags) (impAPort flags)
  addrB <- resolve (impBIP flags) (impBPort flags)
  socA <- open "implementation-A" addrA
  socB <- open "implementation-B" addrB
  sendInstructionTrace socA ([RVFI_DII_Instruction {
                                            padding   = 0,
                                            rvfi_cmd  = rvfi_cmd_end,
                                            rvfi_time = 1,
                                            rvfi_ins_insn = 0
                                          }])
  _ <- receiveExecutionTrace False socA
  sendInstructionTrace socB ([RVFI_DII_Instruction {
                                            padding   = 0,
                                            rvfi_cmd  = rvfi_cmd_end,
                                            rvfi_time = 1,
                                            rvfi_ins_insn = 0
                                          }])
  _ <- receiveExecutionTrace False socB
  addrInstr <- mapM (resolve "127.0.0.1") (instrPort flags)
  instrSoc <- mapM (open "instruction-generator-port") addrInstr
  --
  let checkResult = if (optVerbose flags)
                    then verboseCheckWithResult
                    else quickCheckWithResult
  let checkSingle trace = do
      quickCheckWith (Args Nothing 1 1 2048 True 0) (prop (return trace) socA socB True (timeoutDelay flags))
  let checkGen gen remainingTests = do
      result <- checkResult (Args Nothing remainingTests 1 2048 True 1000) (prop (liftM (map inst_to_rvfi_dii) gen) socA socB (optVerbose flags) (timeoutDelay flags))
      case result of
        Failure {} -> do
          writeFile "last_failure.S" ("# last failing test case:\n" ++ (unlines (failingTestCase result)))
          putStrLn "Replaying shrunk failed test case:"
          checkSingle (read_rvfi_inst_trace (lines(head(failingTestCase result))))
          case (saveDir flags) of
            Nothing -> do
              putStrLn "Save this trace (give file name or leave empty to ignore)?"
              fileName <- getLine
              when (not $ null fileName) $ do
                putStrLn "One-line description?"
                comment <- getLine
                writeFile (fileName ++ ".S") ("# " ++ comment
                                              ++ "\n" ++ (unlines (failingTestCase result)))
              return ()
            Just dir -> do
              writeFile (dir ++ "/failure" ++ (show (remainingTests - (numTests result))) ++ ".S") ("# Automatically generated failing test case" ++ "\n" ++ (unlines (failingTestCase result)))
              checkGen gen (remainingTests - (numTests result))
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
              when ((head archStrings) =~ ("i"::String)) (
                do
                  putStrLn "rvxxi Arithmetic Verification:"
                  checkGen (genTest $ repeatTillEnd genArithmetic)  (nTests flags)
                  putStrLn "rvxxi Memory Verification:"
                  checkGen (genTest $ repeatTillEnd genMemory) (nTests flags)
                  putStrLn "rvxxi Control Flow Verification:"
                  checkGen (genTest $ repeatTillEnd genControlFlow) (nTests flags)
                  putStrLn "rvxxi All Verification:"
                  checkGen (genTest $ repeatTillEnd genAll) (nTests flags)
                  putStrLn "rvxxi Template:"
                  checkGen (genTest $ repeatTillEnd randomTest) (nTests flags)
                  )
              when (elem "cheri" archStrings) (
                do
                  putStrLn "xCHERI Capability Inspection Verification:"
                  checkGen (genTest $ repeatTillEnd genCHERIinspection) (nTests flags)
                  putStrLn "xCHERI Capability Arithmetic Verification:"
                  checkGen (genTest $ repeatTillEnd genCHERIarithmetic) (nTests flags)
                  putStrLn "xCHERI Capability Miscellaneous Verification:"
                  checkGen (genTest $ repeatTillEnd genCHERImisc) (nTests flags)
                  putStrLn "xCHERI Capability Control Flow Verification:"
                  checkGen (genTest $ repeatTillEnd genCHERIcontrol) (nTests flags)
                  putStrLn "xCHERI Template:"
                  checkGen (genTest $ randomCHERITest) (nTests flags)
                  )
            Just sock -> do
              checkGen (listOf $ genInstrServer sock) (nTests flags)
  --
  close socA
  close socB
  --
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open dest addr = do
        putStrLn ("connecting to " ++ dest ++ " ...")
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        return sock

--------------------------------------------------------------------------------
prop :: Gen [RVFI_DII_Instruction] -> Socket -> Socket -> Bool -> Int -> Property
prop gen socA socB doLog timeoutDelay =  forAllShrink gen shrink ( \instTrace -> monadicIO ( run ( do
  let instTraceTerminated = ( instTrace ++ [RVFI_DII_Instruction {
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
  m_modTrace <- timeout timeoutDelay $ receiveExecutionTrace doLog socA
  when (isNothing m_modTrace) $ putStrLn("Error: Timeout")
  when doLog $ putStrLn "Socket B Trace (implementation)"
  when doLog $ putStrLn "-------------------------------"
  m_impTrace <- timeout timeoutDelay $ receiveExecutionTrace doLog socB
  when (isNothing m_impTrace) $ putStrLn("Error: Timeout")

  return $ case (m_modTrace, m_impTrace) of
             (Just modTrace, Just impTrace) -> (and (zipWith (==) modTrace impTrace))
             _                              -> False
  )))

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

-- Receive a fixed number of bytes
receiveBlocking :: Int64 -> Socket -> IO(BS.ByteString)
receiveBlocking n sock = if toInteger(n) == 0 then return empty else do
  received <- recv sock n;
  remainder <- receiveBlocking (n - BS.length(received)) sock
  return $ BS.append received remainder

-- Receive an execution trace
receiveExecutionTrace :: Bool -> Socket -> IO ([RVFI_DII_Execution])
receiveExecutionTrace doLog sock = do
  msg <- receiveBlocking 88 sock
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
