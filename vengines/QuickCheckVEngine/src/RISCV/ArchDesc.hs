--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Alexandre Joannou
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory (Department of Computer Science and
-- Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
-- DARPA SSITH research programme.
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

module RISCV.ArchDesc (ArchDesc(..), archDesc_rv32i, fromString) where

import Text.Regex.Posix ((=~))
import Data.List.Split (splitOneOf)
import Data.Char (toLower)

data ArchDesc = ArchDesc { has_xlen_32 :: Bool
                         , has_xlen_64 :: Bool
                         , has_i       :: Bool
                         , has_m       :: Bool
                         , has_a       :: Bool
                         , has_f       :: Bool
                         , has_icsr    :: Bool
                         , has_ifencei :: Bool
                         , has_cheri   :: Bool
                         } deriving (Show)

archDesc_rv32i = ArchDesc { has_xlen_32 = True
                          , has_xlen_64 = False
                          , has_i       = True
                          , has_m       = False
                          , has_a       = False
                          , has_f       = False
                          , has_icsr    = False
                          , has_ifencei = False
                          , has_cheri   = False
                          }

fromString :: String -> ArchDesc
fromString str = ArchDesc { has_xlen_32 = True
                          , has_xlen_64 = rv64
                          , has_i       = i
                          , has_m       = m
                          , has_a       = a
                          , has_f       = f
                          , has_icsr    = icsr
                          , has_ifencei = ifencei
                          , has_cheri   = cheri
                          }
  where rawSplit = splitOneOf "_zx" (map toLower str)
        archStrings = filter (\x -> not $ null x) rawSplit
        rv64 = (head archStrings) =~ "rv64"
        i = ((head archStrings) =~ "i") || ((head archStrings) =~ "g")
        m = ((head archStrings) =~ "m") || ((head archStrings) =~ "g")
        a = ((head archStrings) =~ "a") || ((head archStrings) =~ "g")
        f = ((head archStrings) =~ "f") || ((head archStrings) =~ "g")
        --d = ((head archStrings) =~ "d") || ((head archStrings) =~ "g")
        icsr = elem "icsr" archStrings || ((head archStrings) =~ "g")
        ifencei = elem "ifencei" archStrings || ((head archStrings) =~ "g")
        cheri = elem "cheri" archStrings
