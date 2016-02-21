{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Main where

import Control.Monad.Trans (liftIO)
import Control.Monad.Random (evalRandIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import System.Directory (getDirectoryContents)
import Lib
import Web.Scotty

main :: IO ()
main =
  listDirectory "data" >>= mapM TIO.readFile >>=
  withDigrams . (digrams . clean . T.concat)

withDigrams :: Digrams -> IO ()
withDigrams (!t) =
  scotty 3111 $ do
    get "/" $ file "./data/index.html"
    get "/text/:length" $ do
      len <- (min 1000) `fmap` param "length"
      result <- liftIO $ evalRandIO (fromDigrams len t)
      text (connect result)

connect :: [T.Text] -> TL.Text
connect = TL.fromChunks . (:[]) . T.intercalate (T.pack " ")

listDirectory :: FilePath -> IO [[Char]]
listDirectory d = getDirectoryContents d >>=
                  return . map ((d ++ "/") ++)
                  . filter (\e -> not (elem e [".", "..", "index.html"]))
