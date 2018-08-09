#!/usr/bin/env stack
module Markov where

import qualified Data.HashMap.Strict as Map
import System.Environment
import System.Random

type Prefix = (String, String)

type Suffix = String

type Chain = Map.HashMap Prefix [Suffix]

groups :: [String] -> [(Prefix, [Suffix])]
groups words = map (\(x, y, z) -> ((x, y), [z])) thruples
  where
    thruples = zip3 words x y
    x = tail words
    y = tail x

makeMap :: [String] -> Chain
makeMap = Map.fromListWith mappend . groups

--random list index into [Suffix] preserves element weights
--
choose :: [a] -> IO a
choose xs = do
  gen <- getStdGen
  newStdGen
  let index = fst $ randomR (0, length xs - 1) gen
  return $ xs !! index

seed :: Chain -> IO Prefix
seed = choose . Map.keys

lookup :: Chain -> Prefix -> String -> IO String
lookup chain p@(_, p2) out = do
  let suffix = choose <$> Map.lookup p chain
  case suffix of
    Nothing -> return out
    Just s -> do
      s <- s
      Markov.lookup chain (p2, s) (out ++ " " ++ s)

generate :: [String] -> IO String
generate source = do
  let chain = makeMap source
  p <- seed chain
  Markov.lookup chain p ""

main :: IO ()
main = do
  (file:_) <- getArgs
  source <- readFile file
  out <- generate $ words source
  putStrLn $ take 800 out
