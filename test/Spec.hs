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

  describe "text can be generated" $ do
    it "from unigrams" $ do
      let t =  ["a", "b", "c", "a", "b", "c"]
          g = mkStdGen 1
      evalRand (fromUnigrams t 2) g `shouldBe` ["c", "a"]

    it "from digrams" $ do
      let t =  ["a", "b", "c", "a", "b", "c"]
          g = mkStdGen 1
      evalRand (fromDigrams t 2) g `shouldBe` ["c", "a", "b"]

    it "from trigrams" $ do
      let t =  ["a", "b", "c", "a", "b", "c"]
          g = mkStdGen 1
      evalRand (fromTrigrams t 2) g `shouldBe` ["c", "a", "b", "c"]
