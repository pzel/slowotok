{-# LANGUAGE BangPatterns, OverloadedStrings, TupleSections #-}
module Main where

import Control.Arrow (second)
import Control.Monad.Trans (liftIO)
import Control.Monad.Random (evalRandIO)
import Data.Default.Class (def)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import Network.HTTP.Types (status400)
import Network.Wai.Middleware.RequestLogger
import System.Directory (getDirectoryContents)
import Lib
import Web.Scotty

main :: IO ()
main = do
  logger <- mkRequestLogger def{outputFormat = Apache FromHeader}
  langs <- subDirs "data"
  wrds <- mapM (\(l,fs) -> mapM TIO.readFile fs >>= return . (l,)) langs
  withNgrams logger (map (second (digrams . clean . T.concat)) wrds)
 where
   withNgrams logger (!ts) = scotty 3111 $ do
     middleware logger
     get "/" $ file "./static/index.html"
     get "/text/:lang/:length" $ do
       len <- (min 1000) `fmap` param "length"
       lang <- param "lang"::ActionM String
       case lookup lang ts of
        Nothing -> do
          text (mconcat ["Language ", TL.pack lang, " not supported"])
          status status400
        (Just ngrams) -> do
          r <- liftIO $ evalRandIO (fromDigrams len ngrams)
          text (connect r)

connect :: [T.Text] -> TL.Text
connect = TL.fromChunks . (:[]) . T.intercalate (T.pack " ")

subDirs :: FilePath -> IO [(String, [String])]
subDirs d = do
  subdirs <- filteredContents d
  mapM (\s -> dirFiles (d </> s) >>= return . (s,) ) subdirs

dirFiles :: FilePath -> IO [String]
dirFiles d =
  filteredContents d >>=
  return . map (d </>) . filter (\e -> not (elem e [".", ".."]))

filteredContents :: FilePath -> IO [FilePath]
filteredContents d =
  filter (not . flip elem [".", ".."]) `fmap` getDirectoryContents d

(</>) :: FilePath -> FilePath -> FilePath
p </> r = p ++ "/" ++ r
