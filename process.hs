#!/usr/bin/env stack
module Main where

import Control.Arrow
import Data.List
import System.Directory
import System.Environment

strip :: String -> String
strip = filter $ not . flip elem ['\t', '\n']

-- for curiosity's sake
freqs :: String -> [(Int, String)]
freqs = map (length &&& head) . group . sort . words

clean :: String -> FilePath -> IO ()
clean "1" _ = return ()
clean _ dir = removeDirectoryRecursive dir

main :: IO ()
main = do
  (dir:flag:_) <- getArgs --combine .txt files in dir, delete? flag
  contents <- getDirectoryContents dir
  let files = filter (isSuffixOf ".txt") contents
  let paths = map ((dir ++ "/") ++) files
  contents <- mapM readFile paths
  writeFile (dir ++ ".txt") $ (strip . unlines) contents
  clean flag dir
