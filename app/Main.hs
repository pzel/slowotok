{-# LANGUAGE BangPatterns, OverloadedStrings, TupleSections #-}
module Main where

import Control.Monad.Random (evalRandIO)
import Control.Monad.Trans (liftIO)
import Data.Default.Class (def)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Network.HTTP.Types (status400)
import Network.Wai.Middleware.RequestLogger
import System.Directory (getDirectoryContents)
import Lib (Corpus, clean, ngrams)
import Web.Scotty

main :: IO ()
main = do
  logger <- mkRequestLogger def{outputFormat = Apache FromHeader}
  lcs <- langCorpuses "./data"
  withNgrams logger lcs
 where
   withNgrams logger (!languageCorpuses) = scotty 3111 $ do
     middleware logger
     get "/" indexFile
     get "/text/:lang/:length" (generateText languageCorpuses)

indexFile :: ActionM ()
indexFile = file "./static/index.html"

generateText :: [(String, Corpus)] -> ActionM ()
generateText ts = do
  len <- (min 1000) `fmap` param "length"
  lang <- param "lang"::ActionM String
  case lookup lang ts of
   Nothing -> do
     text (mconcat ["Language ", T.pack lang, " not supported"])
     status status400
   (Just corpus) -> do
     r <- liftIO $ evalRandIO (ngrams corpus 2 len)
     text (T.intercalate " " r)

langCorpuses :: FilePath -> IO [(String, Corpus)]
langCorpuses d = listContents d >>=
                 mapM (\l -> makeCorpus l (d</>l))
  where
    makeCorpus :: String -> FilePath -> IO (String, Corpus)
    makeCorpus lang dir = listContents dir >>=
                          mapM (TIO.readFile . (dir</>)) >>=
                          return . (lang,) .  clean . T.concat
    listContents :: FilePath -> IO [FilePath]
    listContents dir =
      filter (not . flip elem [".", ".."]) `fmap` getDirectoryContents dir
    (</>) :: FilePath -> FilePath -> FilePath
    p </> r = p ++ "/" ++ r
