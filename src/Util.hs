module Util where

getInput n = do
  let filename = "input/" ++ show n ++ ".txt"
  raw <- readFile filename
  return raw

