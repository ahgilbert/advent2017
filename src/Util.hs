module Util where

import Data.Array
import Data.Graph
import Data.Maybe
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

slurp n = do
  let filename = "input/" ++ show n ++ ".txt"
  raw <- reverse <$> dropWhile (== '\n') <$> reverse <$> readFile filename
  return raw

xslurp n = do
  let filename = "input/x" ++ show n ++ ".txt"
  raw <- reverse <$> dropWhile (== '\n') <$> reverse <$> readFile filename
  return raw

manhattan (a,b) (c,d) =
  let dx = max a c - min a c
      dy = max b d - min b d
  in dx + dy

type Parser = Parsec Void String

num :: Parser Int
num = read <$> (some $ oneOf "-0987654321")

pad n s =
  if length s >= n
  then s
  else (replicate (n - length s) '0') ++ s

add2 (a,b) (c,d) = (a+c, b+d)

neighbors center =
  let steps = [(x,y) | x <- [-1,0,1], y <- [-1,0,1]]
  in filter (\x -> x /= center) $ map (add2 center) steps
