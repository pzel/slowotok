{-# LANGUAGE OverloadedStrings #-}
module Lib
  (Token,Corpus,Frequency,
   digrams, trigrams, clean, frequencies,
   fromDigrams, fromTrigrams
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
   addNext (t,u) m = M.insertWith (++) t [u] m
   shift l = zipWith (,) l (tail l)

trigrams :: Corpus -> TrigramMap
trigrams tokens =
  foldr addNext M.empty (shift tokens)
 where
   addNext :: (Token,Token,Token) -> TrigramMap -> TrigramMap
   addNext (t,u,v) m = M.insertWith (++) (t,u) [v] m
   shift l = zipWith3 (,,) l (tail l) (tail (tail l))

fromDigrams :: (RandomGen g) => Integer -> DigramMap -> Rand g [Token]
fromDigrams k m = do
  key <- randomKey m
  case M.lookup key m of
    Nothing -> error "The impossible has happened"
    (Just ts) -> do
      e <- randomEl ts
      buildText (k-1) m [e,key]
  where
    buildText :: (RandomGen g) =>
                 Integer -> DigramMap -> [Token] -> Rand g [Token]
    buildText 0 _ acc = return (reverse acc)
    buildText k m acc@(t:_) =
      case M.lookup t m of
        Nothing -> return (reverse acc)
        (Just ts) -> do
                     t' <- randomEl ts
                     buildText (k-1) m (t':acc)
    buildText _ _ _ = return []


fromTrigrams :: (RandomGen g) => Integer -> TrigramMap -> Rand g [Token]
fromTrigrams k m = do
  key@(t,v) <- randomKey m
  case M.lookup key m of
    Nothing -> error "The impossible has happened"
    (Just ts) -> do
      e <- randomEl ts
      buildText (k-1) m [e,v,t]
  where
    buildText :: (RandomGen g) =>
                 Integer -> TrigramMap -> [Token] -> Rand g [Token]
    buildText 0 _ acc = return (reverse acc)
    buildText k m acc@(v:t:_) =
      case M.lookup (t,v) m of
        Nothing -> return (reverse acc)
        (Just ts) -> do
                     t' <- randomEl ts
                     buildText (k-1) m (t':acc)
    buildText _ _ _ = return []



randomKey :: (RandomGen g) => M.Map k v -> Rand g k
randomKey m = getRandomR (0, M.size m - 1) >>= return . (M.keys m !!)

randomEl :: (RandomGen g) => [a] -> Rand g a
randomEl l = getRandomR (0, length l - 1) >>= return . (l  !!)

frequency :: Int -> [[Token]] -> Token -> Frequency
frequency n grouped word =
  let t' = dropWhile (\e -> head e /= word) grouped
      count = length $ head t'
  in  fromIntegral count / fromIntegral n
