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
  lcs <- langCorpuses "./data"
  withNgrams logger (map (second digrams) lcs)
 where
   withNgrams logger (!ts) = scotty 3111 $ do
     middleware logger
     get "/" indexFile
     get "/text/:lang/:length" (generateText ts)

indexFile :: ActionM ()
indexFile = file "./static/index.html"

generateText :: [(String, Digrams)] -> ActionM ()
generateText ts = do
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
