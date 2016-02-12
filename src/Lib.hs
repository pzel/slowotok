{-# LANGUAGE OverloadedStrings #-}
module Lib
  (Token,Corpus,Frequency,
   digrams, clean, frequencies
    ) where

import Control.Parallel.Strategies
import Data.Char (isSpace)
import Data.List (group,nub,sort,sortBy)
import qualified Data.Map as M
import qualified Data.Text as T

type Token = T.Text
type Corpus = [Token]
type Frequency = Rational

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
   addNext :: (Token,Token) -> M.Map Token [Token] -> M.Map Token [Token]
   addNext (t,next) map = M.insertWith (++) t [next] map
   shift l = zipWith (,) l (tail l)

frequency :: Int -> [[Token]] -> Token -> Frequency
frequency n grouped word =
  let t' = dropWhile (\e -> head e /= word) grouped
      count = length $ head t'
  in  fromIntegral count / fromIntegral n
