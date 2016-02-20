{-# LANGUAGE OverloadedStrings #-}
module Lib
  (Token,Corpus,
   Digrams, Trigrams,
   digrams, trigrams, clean,
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
type Digrams = M.Map Token [Token]
type Trigrams = M.Map (Token,Token) [Token]

clean :: T.Text -> [Token]
clean = filter (not . junk) . T.split isSpace . T.map replacePunctuation
  where
    replacePunctuation c = if c `elem` ("+„”*—-:;'\"()[]»«"::String)
                           then ' ' else c
    junk e = e `elem` ["", ".", "..", ",", ",,"]

digrams :: Corpus -> Digrams
digrams = let merge (t,u) m = M.insertWith (++) t [u] m
          in ngrams merge pairs

trigrams :: Corpus -> Trigrams
trigrams = let merge (t,u,v) m = M.insertWith (++) (t,u) [v] m
           in ngrams merge triplets

fromDigrams :: (RandomGen g) => Integer -> Digrams -> Rand g [Token]
fromDigrams k m = do
  (key, ts) <- randomKV m
  e <- randomEl ts
  build (k-1) m [e,key]
  where
    build :: (RandomGen g) => Integer -> Digrams -> [Token] -> Rand g [Token]
    build 0 _ acc = return (reverse acc)
    build i d acc@(t:_) = case M.lookup t d of
                            Nothing -> return (reverse acc)
                            (Just ts) -> do
                              t' <- randomEl ts
                              build (i-1) d (t':acc)
    build _ _ _ = return []

fromTrigrams :: (RandomGen g) => Integer -> Trigrams -> Rand g [Token]
fromTrigrams k m = do
  ((t,v), ts) <- randomKV m
  e <- randomEl ts
  build (k-1) m [e,v,t]
  where
    build :: (RandomGen g) => Integer -> Trigrams -> [Token] -> Rand g [Token]
    build 0 _ acc = return (reverse acc)
    build i d acc@(v:t:_) = case M.lookup (t,v) m of
                              Nothing -> return (reverse acc)
                              (Just ts) -> do
                                t' <- randomEl ts
                                build (i-1) d (t':acc)
    build _ _ _ = return []


ngrams :: (a -> M.Map k v -> M.Map k v) -> ([b] -> [a]) -> [b] -> M.Map k v
ngrams add shift tokens = foldr add M.empty (shift tokens)

pairs :: [a] -> [(a,a)]
pairs l = zipWith (,) l (tail l)

triplets :: [a] -> [(a,a,a)]
triplets l = zipWith3 (,,) l (tail l) (tail (tail l))

randomKV :: (RandomGen g) => M.Map k v -> Rand g (k,v)
randomKV m  = getRandomR (0, M.size m - 1) >>= return . (M.assocs m !!)

randomEl :: (RandomGen g) => [a] -> Rand g a
randomEl l = getRandomR (0, length l - 1) >>= return . (l  !!)
