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

  describe "frequencies" $ do
    it "works on singleton corpus" $ do
      frequencies ["hello"] `shouldBe` [("hello", 1)]
    it "works on more words" $ do
      frequencies ["hello", "hello", "world"]
      `shouldBe` [("hello", 2/3), ("world", 1/3)]

  describe "digrams" $ do
    it "works on two word corpus" $ do
      digrams_sorted ["hello", "world"]
        `shouldBe` [("hello", ["world"])]
    it "adds all subsequent strings to the parent key" $ do
      digrams_sorted ["hello", "world", "hello", "moon"]
        `shouldBe` [("hello", ["world", "moon"]),
                    ("world", ["hello"])]

  describe "trigrams" $ do
    it "works on three word corpus" $ do
      trigrams_sorted ["hello", "there", "moon"]
        `shouldBe` [(("hello","there"), ["moon"])]
    it "adds all subsequent strings to the parent key" $ do
      trigrams_sorted ["hello", "there", "moon", "hello", "there", "moon"]
        `shouldBe` [(("hello","there"), ["moon", "moon"]),
                    (("moon","hello"), ["there"]),
                    (("there","moon"), ["hello"])]

  describe "text can be generated" $ do
    it "from trigrams" $ do
      let t = trigrams ["a", "b", "c", "a", "b", "c"]
          g = mkStdGen 1
      evalRand (fromTrigrams 1 t) g `shouldBe` ["c", "a", "b"]
    it "from digrams" $ do
      let t = digrams ["a", "b", "c", "a", "b", "c"]
          g = mkStdGen 1
      evalRand (fromDigrams 1 t) g `shouldBe` ["c", "a"]

digrams_sorted = sort . M.toList . digrams
trigrams_sorted = sort . M.toList . trigrams
