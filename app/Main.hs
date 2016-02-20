{-# LANGUAGE OverloadedStrings #-}
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
main = do
  files <- listDirectory "data"
  t <- (trigrams . clean . T.concat) `fmap` mapM TIO.readFile files
  t `seq`
    scotty 3111 $ do
         get "/text/:length" $ do
                  len <- (min 1000) `fmap` param "length"
                  result <- liftIO $ evalRandIO (fromTrigrams len t)
                  text (connect result)


connect :: [T.Text] -> TL.Text
connect = TL.fromChunks . (:[]) . T.intercalate (T.pack " ")

listDirectory :: FilePath -> IO [[Char]]
listDirectory d = getDirectoryContents d >>=
                  return . map ((d ++ "/") ++)
                  . filter (\e -> not (elem e [".", ".."]))
