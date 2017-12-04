module Util where

getInput n = do
  let filename = "input/" ++ show n ++ ".txt"
  raw <- readFile filename
  return raw

manhattan (a,b) (c,d) =
  let dx = max a c - min a c
      dy = max b d - min b d
  in dx + dy
