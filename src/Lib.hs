{-# LANGUAGE OverloadedStrings #-}
module Lib
  (Token,Corpus,Frequency,
   digrams, trigrams, clean, frequencies,
   fromTrigram
  ) where

import Control.Parallel.Strategies
import Control.Monad.Random
import Data.Char (isSpace)
import Data.List (group,nub,sort)
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

digrams :: Corpus -> DigramMap
digrams tokens =
  foldr addNext M.empty (shift tokens)
 where
   addNext :: (Token,Token) -> DigramMap -> DigramMap
   addNext (t,next) m = M.insertWith (++) t [next] m
   shift l = zipWith (,) l (tail l)

trigrams :: Corpus -> TrigramMap
trigrams tokens =
  foldr addNext M.empty (shift tokens)
 where
   addNext :: (Token,Token,Token) -> TrigramMap -> TrigramMap
   addNext (t,u,v) m = M.insertWith (++) (t,u) [v] m
   shift l = zipWith3 (,,) l (tail l) (tail (tail l))


fromTrigram :: (RandomGen g) => TrigramMap -> Rand g [Token]
fromTrigram m = do
  let l = M.toList m
      len = length l
  idx <- getRandomR (0,len-1)
  let randomKey = fst (l !! idx)
      entry = M.findWithDefault [] randomKey m
  case entry of
    [] -> return []
    tokens -> return tokens

frequency :: Int -> [[Token]] -> Token -> Frequency
frequency n grouped word =
  let t' = dropWhile (\e -> head e /= word) grouped
      count = length $ head t'
  in  fromIntegral count / fromIntegral n
