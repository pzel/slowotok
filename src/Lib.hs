{-# LANGUAGE OverloadedStrings #-}
module Lib
  (Token,Corpus,Frequency,
   digrams, trigrams, clean, frequencies
  ) where

import Control.Parallel.Strategies
import Data.Char (isSpace)
import Data.List (group,nub,sort,sortBy)
import qualified Data.Map as M
import qualified Data.Text as T

type Token = T.Text
type Corpus = [Token]
type Frequency = Rational
type DigramMap = M.Map Token [Token]
type TrigramMap = M.Map (Token,Token) [Token]

clean :: T.Text -> [Token]
clean = filter (not . junk) . T.split isSpace . T.map replacePunctuation
  where
    replacePunctuation c = if c `elem` ("+„”*—-:;'\"()[]»«"::String)
                           then ' ' else c
    junk e = e `elem` ["", ".", "..", ",", ",,"]

frequencies :: [Token] -> [(Token, Frequency)]
frequencies tokens =
  let sorted = sort tokens
      grouped = group sorted
      n = length tokens
      wordsToCheck = nub sorted
      findFreq = \t -> (t, frequency n grouped t)
  in map findFreq wordsToCheck `using` parList rdeepseq

digrams :: Corpus -> [(Token, [Token])]
digrams tokens =
  M.toList (foldr addNext M.empty (shift tokens))
 where
   addNext :: (Token,Token) -> DigramMap -> DigramMap
   addNext (t,next) map = M.insertWith (++) t [next] map
   shift l = zipWith (,) l (tail l)

trigrams :: Corpus -> [((Token,Token), [Token])]
trigrams tokens =
  M.toList (foldr addNext M.empty (shift tokens))
 where
   addNext :: (Token,Token,Token) -> TrigramMap -> TrigramMap
   addNext (t,u,v) map = M.insertWith (++) (t,u) [v] map
   shift l = zipWith3 (,,) l (tail l) (tail (tail l))

frequency :: Int -> [[Token]] -> Token -> Frequency
frequency n grouped word =
  let t' = dropWhile (\e -> head e /= word) grouped
      count = length $ head t'
  in  fromIntegral count / fromIntegral n
