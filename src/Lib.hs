{-# LANGUAGE OverloadedStrings #-}
module Lib
  (Token,Corpus,
   Unigrams, Digrams, Trigrams,
   unigrams, digrams, trigrams, clean,
   fromUnigrams, fromDigrams, fromTrigrams
  ) where

import Control.Monad.Random
import Data.Char (isSpace)
import Data.List (foldl', zipWith4)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

type Token = T.Text
type Corpus = [Token]
type Unigrams = M.Map Token [Token]
type Digrams = M.Map (Token,Token) [Token]
type Trigrams = M.Map (Token,Token,Token) [Token]

clean :: T.Text -> Corpus
clean = filter (not . junk) . T.split isSpace . T.map replacePunctuation
  where
    replacePunctuation c = if c `elem` ("…+„”*—-:;'\"()[]»«"::String)
                           then ' ' else c
    junk e = e `elem` ["", ".", ",", "..", ",,"]

unigrams :: Corpus -> Unigrams
unigrams = let merge (t,u) m = M.insertWith (++) t [u] m
          in ngrams merge pairs

digrams :: Corpus -> Digrams
digrams = let merge (t,u,v) m = M.insertWith (++) (t,u) [v] m
           in ngrams merge triplets

trigrams :: Corpus -> Trigrams
trigrams = let merge (t,u,v,w) m = M.insertWith (++) (t,u,v) [w] m
           in ngrams merge quadruplets

fromUnigrams :: (RandomGen g) => Integer -> Unigrams -> Rand g [Token]
fromUnigrams k m = fromNGrams (\t e -> [e,t]) (\(t:_) -> t) k m

fromDigrams :: (RandomGen g) => Integer -> Digrams -> Rand g [Token]
fromDigrams k m = fromNGrams (\(t,v) e -> [e,v,t]) (\(v:t:_) -> (t,v)) k m

fromTrigrams :: (RandomGen g) => Integer -> Trigrams -> Rand g [Token]
fromTrigrams k m = fromNGrams (\(t,v,u) e -> [e,u,v,t]) (\(u:v:t:_) -> (t,v,u)) k m

fromNGrams ::
  (RandomGen g, Ord n) =>
  (n -> Token -> [Token]) -> ([Token] -> n) -> Integer -> M.Map n [Token]
  -> Rand g [Token]
fromNGrams initialBuildStep accDecompStep k m = do
  (key,tokens) <- randomKV m
  e <- randomEl tokens
  build (k-1) (initialBuildStep key e)
 where
    build 0 acc = return (reverse acc)
    build i acc = case M.lookup (accDecompStep acc) m of
                   Nothing -> return (reverse acc)
                   (Just ts) -> randomEl ts >>= build (i-1) . (:acc)

ngrams :: (a -> M.Map k v -> M.Map k v) -> ([b] -> [a]) -> [b] -> M.Map k v
ngrams add shift tokens = foldl' (flip add) M.empty (shift tokens)

pairs :: [a] -> [(a,a)]
pairs l = zipWith (,) l (tail l)

triplets :: [a] -> [(a,a,a)]
triplets l = zipWith3 (,,) l (tail l) (tail (tail l))

quadruplets :: [a] -> [(a,a,a,a)]
quadruplets l = zipWith4 (,,,) l (tail l) (tail (tail l)) (tail (tail (tail l)))

randomKV :: (RandomGen g) => M.Map k v -> Rand g (k,v)
randomKV m  = getRandomR (0, M.size m - 1) >>= return . (M.assocs m !!)

randomEl :: (RandomGen g) => [a] -> Rand g a
randomEl l = getRandomR (0, length l - 1) >>= return . (l  !!)
