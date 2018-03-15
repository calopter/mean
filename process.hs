#!/usr/bin/env stack
module Main where

import Control.Arrow
import Data.List
import System.Directory
import System.Environment

strip :: String -> String
strip = filter $ not . flip elem ['\t', '\n']

freqs :: String -> [(Int, String)]
freqs = map (length &&& head) . group . sort . words

main :: IO ()
main = do
  (songName:_) <- getArgs
  let dir = "./out/radiohead/" ++ songName
  files <- getDirectoryContents dir
  let paths = map ((dir ++ "/") ++) $ drop 2 $ sort files
  contents <- mapM readFile paths
  writeFile (dir ++ ".txt") $ (strip . unlines) contents
  removeDirectoryRecursive dir
