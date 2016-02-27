{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Random (evalRand)
import System.Random (mkStdGen)
import qualified Data.Map as M
import Data.List (sort)
import Lib
import Test.Hspec

main = hspec tests

tests = do
  describe "cleaning text" $ do
    it "removes spurious puctuation" $ do
      clean "hello, ,, there!" `shouldBe` ["hello,","there!"]

  describe "unigrams" $ do
    it "works on two word corpus" $ do
      unigrams_sorted ["hello", "world"]
        `shouldBe` [("hello", ["world"])]
    it "adds all subsequent strings to the parent key" $ do
      unigrams_sorted ["hello", "world", "hello", "moon"]
        `shouldBe` [("hello", ["moon", "world"]),
                    ("world", ["hello"])]

  describe "digrams" $ do
    it "works on three word corpus" $ do
      digrams_sorted ["hello", "there", "moon"]
        `shouldBe` [(("hello","there"), ["moon"])]
    it "adds all subsequent strings to the parent key" $ do
      digrams_sorted ["hello", "there", "moon", "hello", "there", "moon"]
        `shouldBe` [(("hello","there"), ["moon", "moon"]),
                    (("moon","hello"), ["there"]),
                    (("there","moon"), ["hello"])]

  describe "trigrams" $ do
    it "works on four word corpus" $ do
      trigrams_sorted ["one", "two", "three", "four"]
        `shouldBe` [(("one", "two", "three"), ["four"])]

  describe "text can be generated" $ do
    it "from unigrams" $ do
      let t = unigrams ["a", "b", "c", "a", "b", "c"]
          g = mkStdGen 1
      evalRand (fromUnigrams 1 t) g `shouldBe` ["c", "a"]

    it "from digrams" $ do
      let t = digrams ["a", "b", "c", "a", "b", "c"]
          g = mkStdGen 1
      evalRand (fromDigrams 1 t) g `shouldBe` ["c", "a", "b"]

    it "from trigrams" $ do
      let t = trigrams ["a", "b", "c", "a", "b", "c"]
          g = mkStdGen 1
      evalRand (fromTrigrams 1 t) g `shouldBe` ["c", "a", "b", "c"]


unigrams_sorted = sort . M.toList . unigrams
digrams_sorted = sort . M.toList . digrams
trigrams_sorted = sort . M.toList . trigrams
