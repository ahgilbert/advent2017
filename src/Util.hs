module Util where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

slurp n = do
  let filename = "input/" ++ show n ++ ".txt"
  raw <- readFile filename
  return raw

manhattan (a,b) (c,d) =
  let dx = max a c - min a c
      dy = max b d - min b d
  in dx + dy

type Parser = Parsec Void String

num :: Parser Int
num = read <$> (some $ oneOf "-0987654321")
