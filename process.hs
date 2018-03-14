module Main where

import Control.Arrow
import Data.List
import System.Directory

strip :: String -> String
strip = filter $ not . flip elem ['\t', '\n']

freqs :: String -> [(Int, String)]
freqs = map (length &&& head) . group . sort . words

main :: IO ()
main = do
  files <- getDirectoryContents "./out/"
  let paths = map ("./out/" ++) $ drop 2 $ sort files
  contents <- mapM readFile paths
  writeFile "out.txt" $ (strip . unlines) contents
