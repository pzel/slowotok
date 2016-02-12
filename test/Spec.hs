{-# LANGUAGE OverloadedStrings #-}
module Main where

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
