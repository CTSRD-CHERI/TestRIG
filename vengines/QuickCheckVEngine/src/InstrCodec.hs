--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- All rights reserved.
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

-- Instruction encoding and decoding

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module InstrCodec where

import Data.Char
import Data.List
import Data.Maybe
import Data.Bits
import Test.QuickCheck

type BitList = [Bool]

data Token =
    Lit String
  | Var String
  | Range String Int Int
  deriving (Show)

tokenise :: String -> [Token]
tokenise = init [] 
  where
    isBit c = c == '0' || c == '1'

    init acc [] = acc
    init acc (c:cs)
      | isSpace c = init acc cs
      | isBit c = lit acc (c:cs)
      | otherwise = var "" acc (c:cs)

    var str acc [] = init (Var (reverse str) : acc) []
    var str acc (c:cs)
      | c == '[' = high (reverse str) acc cs 
      | c == ' ' = init (Var (reverse str) : acc) cs
      | otherwise = var (c:str) acc cs

    high id acc cs =
      case takeWhile isDigit cs of
        [] -> error "Format error: expected high number"
        ds -> colon id n acc (dropWhile isDigit cs)
          where n = read ds :: Int

    colon id high acc (':':cs) = low id high acc cs
    colon id high acc (']':cs) = init (Range id high high : acc) cs
    colon id high acc other = error "Format error: expected ':'"

    low id high acc cs =
      case takeWhile isDigit cs of
        [] -> error "Format error: expected low number"
        ds -> if   n > high
              then error "Format error: range error"
              else close (Range id high n : acc) (dropWhile isDigit cs)
          where n = read ds :: Int

    close acc (']':cs) = init acc cs
    close acc other = error "Format error: expected ']'"

    lit acc cs = init (Lit s : acc) (dropWhile isBit cs)
      where s = reverse (takeWhile isBit cs)

-- A token tagged with the bit offset of it's LSB within the pattern
data TaggedToken = Tag Int Token
  deriving Show

tag :: [Token] -> [TaggedToken]
tag = tagger 0
  where
    tagger n [] = []
    tagger n (t:ts) =
      case t of
        Lit bs -> Tag n t : tagger (n + length bs) ts
        Var v -> error "tag: unranged vars not supported"
        Range v hi lo -> Tag n t : tagger (n + (hi-lo) + 1) ts

-- Mapping from var bit-index to subject bit-index
type Mapping = [(Int, Int)]

mapping :: String -> [TaggedToken] -> Mapping
mapping v toks =
  concat [ zip [lo..hi] [n..]
         | Tag n (Range w hi lo) <- toks, v == w ]

-- Perform a substitution on a subject
subst :: Mapping -> BitList -> BitList
subst m bs = unscatter [(bi, bs !! si) | (bi, si) <- m]

-- Join a scattered bit-string, complain if gaps or overlapping
unscatter :: [(Int, a)] -> [a]
unscatter = join 0
  where
   join i [] = []
   join i m =
     case [x | (j, x) <- m, i == j] of
       [] -> error "Format error: non-contiguous variable assignment"
       [x] -> x : join (i+1) [p | p <- m, fst p /= i]
       other -> error "Format error: overlapping variable assignment"

-- Determine argument values to right-hand-side
args :: BitList -> [TaggedToken] -> [BitList]
args subj = get . reverse
  where
    notVar v (Tag i (Range w hi lo)) = v /= w
    notVar v other = False

    get [] = []
    get ts@(Tag i (Range v hi lo) : rest) =
      subst (mapping v ts) subj :
        get (filter (notVar v) rest)
    get (t:ts) = get ts

-- Determine width of a token
tokenWidth :: Token -> Int
tokenWidth (Var v) = error "Error: tokenWidth not defined for unranged vars"
tokenWidth (Range v hi lo) = (hi-lo)+1
tokenWidth (Lit bs) = length bs

-- Match literals in pattern against subject
matches :: BitList -> [Token] -> Bool
matches subj toks
  | width /= length subj = error "Format error: width mismatch"
  | otherwise = check 0 toks
  where
    width = sum (map tokenWidth toks)

    check n [] = True
    check n (t : rest) =
      case t of
        Var v -> error "Format error: unranged vars not supported"
        Range id hi lo -> check (n + (hi-lo) + 1) rest
        Lit bs ->
             and [ if c == '0' then not b else b
                 | (c, b) <- zip bs (drop n subj) ]
          && check (n + length bs) rest

-- Convert Integer to BitList
toBitList :: Integer -> BitList
toBitList 0 = []
toBitList x = odd x : toBitList (x `shiftR` 1)

infix 9 #
(#) :: Int -> Integer -> BitList
n # x
  | len <= n  = bs ++ replicate (n-len) False
  | otherwise = take n bs
  where
    bs  = toBitList x
    len = length bs

-- Convert BitList to Integer
fromBitList :: BitList -> Integer
fromBitList [] = 0
fromBitList (x:xs) = (if x then 1 else 0) + 2 * fromBitList xs

class Apply f a where
  apply :: f -> [BitList] -> a

instance Apply f f where
  apply f [] = f
  apply f other = error "Format error: too many pattern vars"

instance Apply f a => Apply (Integer -> f) a where
  apply f [] = error "Format error: too few pattern vars"
  apply f (arg:args) = apply (f (fromBitList arg)) args

decodeOne :: Apply f a => String -> f -> (Integer, Int) -> Maybe a
decodeOne fmt rhs = 
  let toks = tokenise fmt
  in  \(subj, w) ->
        let
          subj' = w#subj
        in 
          if   matches subj' toks
          then Just $ apply rhs (args subj' (tag toks))
          else Nothing

infix 9 -->
(-->) :: Apply f a => String -> f -> (Integer, Int) -> Maybe a
pat --> rhs = decodeOne pat rhs

decode :: Int -> Integer -> [(Integer, Int) -> Maybe a] -> a
decode n subj alts =
  case catMaybes [alt (subj, n) | alt <- alts] of
    [] -> error "Codec.decode: pattern match failed"
    match:rest -> match

rangedVars :: [Token] -> [String]
rangedVars toks = nub [v | Range v hi lo <- reverse toks]

scatter :: [Token] -> [(String, Integer)] -> BitList
scatter [] env = []
scatter (tok:toks) env =
  case tok of
    Lit bs -> [b == '1' | b <- bs] ++ scatter toks env
    Var v -> error "Codec.scatter: unranged vars not supported"
    Range v hi lo -> 
      case lookup v env of
        Nothing -> error ("Unknown variable " ++ v)
        Just i  -> let bs = toBitList i ++ repeat False in
                        drop lo (take (hi+1) bs)
                     ++ scatter toks env

class Encode a b | b -> a where
  enc :: [a] -> String -> b

instance Encode Integer Integer where
  enc args fmt = fromBitList (scatter toks (zip (rangedVars toks) args))
    where toks = tokenise fmt

instance Encode (Gen Integer) (Gen Integer) where
  enc args fmt = do
      vals <- sequence args
      return $ fromBitList (scatter toks (zip (rangedVars toks) vals))
    where toks = tokenise fmt

instance Encode a b => Encode a (a -> b) where
  enc args fmt a = enc (args ++ [a]) fmt

encode :: Encode a b => String -> b
encode = enc []
