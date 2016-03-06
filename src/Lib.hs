{-# LANGUAGE OverloadedStrings #-}
module Lib (Token,Corpus,clean,ngrams,
            fromUnigrams,fromDigrams,fromTrigrams) where

import Control.Monad.Random
import Data.Char (isSpace)
import Data.MarkovChain
import qualified Data.Text.Lazy as T

type Token = T.Text
type Corpus = [Token]

clean :: T.Text -> Corpus
clean = filter (not . junk) . T.split isSpace . T.map replacePunctuation
  where
    replacePunctuation c = if c `elem` ("…+„”*—-:;\"()[]»«_"::String)
                           then ' ' else c
    junk e = e `elem` ["", ".", ",", "..", ",,"]

fromUnigrams, fromDigrams, fromTrigrams ::
  (RandomGen g) => Corpus -> Int -> Rand g [Token]
fromUnigrams c len = ngrams c 1 len
fromDigrams c len = ngrams c 2 len
fromTrigrams c len = ngrams c 3 len

ngrams :: (RandomGen g) => Corpus -> Int -> Int -> Rand g [Token]
ngrams c n len = do
  idx <- randomIdx c
  liftRand (\g-> (take len (run n c idx g), g))

randomIdx :: (RandomGen g) => [a] -> Rand g Int
randomIdx l = getRandomR (0, length l - 1)
