--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Peter Rugg
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

module Template where

import Test.QuickCheck

data Template = Empty | Single Integer | Distribution [(Int, Template)] | Sequence [Template] | Random (Gen Template)

instance Show Template where
  show Empty = "Empty"
  show (Single x) = "Single (" ++ (show x) ++ ")"
  show (Distribution x) = "Distribution " ++ (show x)
  show (Sequence x) = "Sequence " ++ (show x)
  show (Random x) = "Random (?)"

--Turn a test template into a single instance of a test
genTest :: Template -> Gen [Integer]
genTest x = do {y <- genTestUnsized x; size <- getSize; return $ take size y} --TODO make this an argument

genTestUnsized :: Template -> Gen [Integer]
genTestUnsized (Empty) = return []
genTestUnsized (Single x) = return [x]
genTestUnsized (Distribution xs) = do {y <- frequency (map (\(a,b) -> (a, return b)) xs); genTest y}
genTestUnsized (Sequence x) = genSeq x
genTestUnsized (Random x) = do {y <- x; genTest y}

genSeq :: [Template] -> Gen [Integer]
genSeq [] = return []
genSeq (x:xs) = do {y <- genTest x; ys <- genSeq xs; return $ y ++ ys}

--Turn a list of possibilities into a uniform distribution
uniform :: [Integer] -> Template
uniform options = Distribution $ map (\x -> (1, Single x)) options

repeatTest :: Int -> Template -> Template
repeatTest n temp = Sequence $ replicate n temp

-- Note that this requires the argument to always return a list of length 1
repeatTillEnd :: Template -> Template
repeatTillEnd temp = Random $ do {
  size <- getSize;
  return $ repeatTest size temp}
