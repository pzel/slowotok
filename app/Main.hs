module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getDirectoryContents)
import Lib

main :: IO ()
main = do
  files <- listDirectory "data"
  t <- T.concat `fmap` mapM TIO.readFile (take 2 files)
  TIO.writeFile "cleaned" (T.pack (show (frequencies (clean t))))

listDirectory d = getDirectoryContents d >>=
                  return . map ((d ++ "/") ++)
                  . filter (\e -> not (elem e [".", ".."]))
