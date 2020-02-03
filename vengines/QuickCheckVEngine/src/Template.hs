--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Peter Rugg
-- Copyright (c) 2020 Alexandre Joannou
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

module Template (
  Template(..)
, instSeq
, instDist
, uniformTemplate
, replicateTemplate
, repeatTemplate
, repeatTemplateTillEnd
, genTemplate
, genTemplateSized
, genTemplateUnsized
, (<>)
) where

import Test.QuickCheck
import Data.List
import Data.Semigroup

-- | 'Template' type to describe sequences of instructions (represented as
--   'Integer's) to be used as tests
data Template = Empty
              | Single Integer
              | Distribution [(Int, Template)]
              | Sequence [Template]
              | Random (Gen Template)
instance Show Template where
  show Empty = "Empty"
  show (Single x) = "Single (" ++ (show x) ++ ")"
  show (Distribution x) = "Distribution " ++ (show x)
  show (Sequence x) = "Sequence " ++ (show x)
  show (Random x) = "Random (?)"
instance Semigroup Template where
  x <> y = Sequence [x, y]
instance Monoid Template where
  mempty = Empty

-- | Turn a list of 'Integer' instructions into a 'Sequence [Template]'
instSeq :: [Integer] -> Template
instSeq insts = Sequence (map Single insts)

-- | Turn a list of '(Int, Integer)' weigthed instructions into a
--   'Distribution [(Int, Template)]'
instDist :: [(Int, Integer)] -> Template
instDist winsts = Distribution $ map (\(w, x) -> (w, Single x)) winsts

-- | Turn a list of possibilities into a uniform distribution
class UniformTemplate t where
  uniformTemplate :: [t] -> Template
instance UniformTemplate Template where
  uniformTemplate options = Distribution $ map (\x -> (1, x)) options
instance UniformTemplate Integer where
  uniformTemplate options = Distribution $ map (\x -> (1, Single x)) options

-- | Replicate a 'Template' a given number of times
replicateTemplate :: Int -> Template -> Template
replicateTemplate n template = Sequence $ replicate n template

-- | Repeat a 'Template' an infinite number of times
repeatTemplate :: Template -> Template
repeatTemplate template = Sequence $ repeat template

-- | Note that this requires the argument to always return a list of length 1
repeatTemplateTillEnd :: Template -> Template
repeatTemplateTillEnd template = Random $ do
  size <- getSize
  return $ replicateTemplate size template

-- | Turn a 'Template' into a single QuickCheck 'Gen [Integer]' generator
--   of list of instructions
genTemplate :: Template -> Gen [Integer]
genTemplate template = getSize >>= genTemplateSized template

-- | Same as 'genTemplate' but specify the desired size
genTemplateSized :: Template -> Int -> Gen [Integer]
genTemplateSized template size = take size <$> genHelper template

-- | Inner helper to implement the 'genTemplate' functions
genHelper :: Template -> Gen [Integer]
genHelper Empty = return []
genHelper (Single x) = return [x]
genHelper (Distribution xs) = do let xs' = map (\(a, b) -> (a, return b)) xs
                                 frequency xs' >>= genHelper
genHelper (Sequence []) = return []
genHelper (Sequence (x:xs)) = do x'  <- genHelper x
                                 xs' <- genHelper $ Sequence xs
                                 return $ x' ++ xs'
genHelper (Random x) = x >>= genHelper
