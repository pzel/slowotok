module Main where

import Control.Monad.Random (evalRandIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getDirectoryContents)
import Lib

main :: IO ()
main = do
  files <- listDirectory "data"
  t <- (trigrams . clean . T.concat) `fmap` mapM TIO.readFile files
  result <- evalRandIO (fromTrigram 100 t)
  TIO.putStrLn (T.intercalate (T.pack " ") result)

listDirectory :: FilePath -> IO [[Char]]
listDirectory d = getDirectoryContents d >>=
                  return . map ((d ++ "/") ++)
                  . filter (\e -> not (elem e [".", ".."]))
