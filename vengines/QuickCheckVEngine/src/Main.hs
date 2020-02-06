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
import Data.IORef
import Text.Printf
import Text.Regex.Posix
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Environment
import System.FilePath.Find
import System.Console.GetOpt
import System.IO.Unsafe
import Data.Maybe ( isJust, isNothing, fromMaybe )
import Data.List
import Data.List.Split
import System.Exit
import System.IO
import RVFI_DII
import RISCV
import Template
import System.Timeout
import System.Exit
import Control.Exception

import Templates.Utils
import Templates.GenAll
import Templates.GenArithmetic
import Templates.GenMemory
import Templates.GenCSRs
import Templates.RandomTest
import Templates.GenControlFlow
import Templates.GenMulDiv
import Templates.GenAtomics
import Templates.GenFP
import Templates.GenCHERI

-- command line arguments
--------------------------------------------------------------------------------
data Options = Options
    { optVerbose     :: Bool
    , nTests         :: Int
    , impAPort       :: String
    , impAIP         :: String
    , impBPort       :: String
    , impBIP         :: String
    , instTraceFile  :: Maybe FilePath
    , instDirectory  :: Maybe FilePath
    , memoryInitFile :: Maybe FilePath
    , arch           :: ArchDesc
    , instrPort      :: Maybe String
    , saveDir        :: Maybe FilePath
    , timeoutDelay   :: Int
    } deriving Show

defaultOptions = Options
    { optVerbose     = False
    , nTests         = 100
    , impAPort       = "5000"
    , impAIP         = "127.0.0.1"
    , impBPort       = "5001"
    , impBIP         = "127.0.0.1"
    , instTraceFile  = Nothing
    , instDirectory  = Nothing
    , memoryInitFile = Nothing
    , arch           = archDesc_rv32i
    , instrPort      = Nothing
    , saveDir        = Nothing
    , timeoutDelay   = 6000000000 -- 60 seconds
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
  , Option ['m']     ["memory-init-file"]
      (ReqArg (\ f opts -> opts { memoryInitFile = Just f }) "PATH")
        "Specify PATH a processed objdump file (e.g. 8000f420 2ed632d8 36da3adc 3edec2c0 c6c2cac4) to use as the initial contents of memory"
  , Option ['r']     ["architecture"]
      (ReqArg (\ f opts -> opts { arch = fromString f }) "ARCHITECTURE")
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
  let archDesc = arch flags
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
  alive <- newIORef True -- Cleared when either implementation times out, since they will may not be able to respond to future queries
  let checkResult = if (optVerbose flags)
                    then verboseCheckWithResult
                    else quickCheckWithResult
  let checkSingle trace verbose doShrink = do
        quickCheckWith (Args Nothing 1 1 2048 True (if doShrink then 1000 else 0)) (prop (return trace) socA socB verbose archDesc (timeoutDelay flags) alive)
  let checkGen gen remainingTests = do
        result <- checkResult (Args Nothing remainingTests 1 2048 True 1000) (prop gen socA socB (optVerbose flags) archDesc (timeoutDelay flags) alive)
        case result of
          Failure {} -> do
            writeFile "last_failure.S" ("# last failing test case:\n" ++ (unlines (failingTestCase result)))
            putStrLn "Replaying shrunk failed test case:"
            checkSingle (read_rvfi_inst_trace (lines(head(failingTestCase result)))) True False
            case (saveDir flags) of
              Nothing -> do
                putStrLn "Save this trace (give file name or leave empty to ignore)?"
                fileName <- getLine
                when (not $ null fileName) $ do
                  putStrLn "One-line description?"
                  comment <- getLine
                  writeFile (fileName ++ ".S") ("# " ++ comment
                                                ++ "\n" ++ (unlines (failingTestCase result)))
                return 1
              Just dir -> do
                writeFile (dir ++ "/failure" ++ (show (remainingTests - (numTests result))) ++ ".S") ("# Automatically generated failing test case" ++ "\n" ++ (unlines (failingTestCase result)))
                checkGen gen (remainingTests - (numTests result))
          other -> return 0
  let checkFile (memoryInitFile :: Maybe FilePath) (fileName :: FilePath) = do
        putStrLn $ "Reading trace from " ++ fileName
        trace <- read_rvfi_inst_trace_file fileName
        initTrace <- case (memoryInitFile) of
          Just memInit -> do putStrLn $ "Reading memory initialisation from file " ++ memInit
                             read_rvfi_data_file memInit
          Nothing -> return mempty
        checkSingle (initTrace <> trace) (optVerbose flags) True
  --
  success <- newIORef 0
  let doCheck a b = do result <- checkGen a b
                       modifyIORef success ((+) result)
  case (instTraceFile flags) of
    Just fileName -> do
      checkFile (memoryInitFile flags) fileName
    Nothing -> do
      case (instDirectory flags) of
        Just directory -> do
          fileNames <- System.FilePath.Find.find always (extension ==? ".S") directory
          mapM_ (checkFile Nothing) fileNames
        Nothing -> do
          case instrSoc of
            Nothing -> do
              when (has_i archDesc) $
                do putStrLn "rv32 I Arithmetic Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_arithmetic) (nTests flags)
                   putStrLn "rv32 I Memory Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_memory) (nTests flags)
                   putStrLn "rv32 I Control Flow Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_controlflow) (nTests flags)
              when (has_i archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 I Arithmetic Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_i_arithmetic) (nTests flags)
                   putStrLn "rv64 I Memory Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_i_memory) (nTests flags)
                   -- Note: no rv64 specific control flow instructions
              when (has_m archDesc) $
                do putStrLn "rv32 M extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_m) (nTests flags)
              when (has_m archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 M extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_m) (nTests flags)
              when (has_a archDesc) $
                do putStrLn "rv32 A extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_a) (nTests flags)
                   putStrLn "rv32 A Memory Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_a_memory) (nTests flags)
              when (has_a archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 A extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_a) (nTests flags)
                   putStrLn "rv64 A Memory Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_i_a_memory) (nTests flags)
              when (has_f archDesc) $
                do putStrLn "rv32 F extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_f) (nTests flags)
              when (has_f archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 F extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_f) (nTests flags)
              when (has_d archDesc) $
                do putStrLn "rv32 D extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_d) (nTests flags)
              when (has_d archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 D extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_d) (nTests flags)
              when (has_icsr archDesc) $
                do putStrLn "rv32 Zicsr extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_zicsr) (nTests flags)
              when (has_ifencei archDesc) $
                do putStrLn "rv32 Zifencei extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_zifencei_memory) (nTests flags)
              when (has_ifencei archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 Zifencei extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_i_zifencei_memory) (nTests flags)
              when (has_cheri archDesc) $
                do putStrLn "Xcheri extension Capability Inspection Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd genCHERIinspection) (nTests flags)
                   putStrLn "Xcheri extension Capability Arithmetic Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd genCHERIarithmetic) (nTests flags)
                   putStrLn "Xcheri extension Capability Miscellaneous Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd genCHERImisc) (nTests flags)
                   putStrLn "Xcheri extension Capability Control Flow Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd genCHERIcontrol) (nTests flags)
                   putStrLn "Xcheri extension Random Template:"
                   doCheck (genTemplate $ randomCHERITest archDesc) (nTests flags)

              putStrLn "All Verification:"
              doCheck (genTemplate $ repeatTemplateTillEnd (genAll archDesc)) (nTests flags)
              putStrLn "Random Template:"
              doCheck (genTemplate $ repeatTemplateTillEnd (randomTest archDesc)) (nTests flags)
            Just sock -> do
              doCheck (liftM toTestCase $ listOf $ liftM (\x -> TS False x) $ listOf $ genInstrServer sock) (nTests flags)
  --
  close socA
  close socB
  --
  exitCode <- readIORef success
  if exitCode == 0 then exitSuccess else exitWith $ ExitFailure exitCode
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
prop :: Gen TestCase -> Socket -> Socket -> Bool -> ArchDesc -> Int -> IORef Bool -> Property
prop gen socA socB doLog arch timeoutDelay alive = forAllShrink gen shrink mkProp
  where mkProp testCase = monadicIO $ run $ do
          let instTrace = map inst_to_rvfi_dii $ fromTestCase testCase
          let instTraceTerminated = instTrace ++ [RVFI_DII_Instruction {
                                                  padding   = 0,
                                                  rvfi_cmd  = rvfi_cmd_end,
                                                  rvfi_time = 1,
                                                  rvfi_ins_insn = 0
                                                }]
          currentlyAlive <- readIORef alive
          if currentlyAlive then do
            result <- try $ do
              -- Send to implementations
              sendInstructionTrace socA instTraceTerminated
              when doLog $ putStrLn "Done sending instructions to implementation A"
              sendInstructionTrace socB instTraceTerminated
              when doLog $ putStrLn "Done sending instructions to implementation B"
              -- Receive from implementations
              when doLog $ putStrLn "----------------------------------------------------------------------"
              when doLog $ putStrLn "Socket A Trace (model)"
              when doLog $ putStrLn "----------------------"
              m_modTrace <- timeout timeoutDelay $ receiveExecutionTrace doLog socA
              when doLog $ putStrLn "Socket B Trace (implementation)"
              when doLog $ putStrLn "-------------------------------"
              m_impTrace <- timeout timeoutDelay $ receiveExecutionTrace doLog socB
              --
              return (m_modTrace, m_impTrace)
            case result of
              Right (Just modTrace, Just impTrace) ->
                return $ property $ Data.List.and (zipWith (checkEq (has_xlen_64 arch)) modTrace impTrace)
              Right (a, b) -> do
                writeIORef alive False
                when (isNothing a) $ putStrLn "Error: implementation A timeout. Forcing all future tests to report 'SUCCESS'"
                when (isNothing b) $ putStrLn "Error: implementation B timeout. Forcing all future tests to report 'SUCCESS'"
                return $ property False
              Left (SomeException e) -> do
                writeIORef alive False
                putStrLn "Error: exception on IO with implementations. Forcing all future tests to report 'SUCCESS'"
                return $ property False
          else do
            when doLog $ putStrLn "Warning: reporting success since implementations not running"
            return $ property True -- We don't want to shrink once one of the implementations has died, so always return that the property is true

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
  received  <- recv sock n
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
