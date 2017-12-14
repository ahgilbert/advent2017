module P13 where

import Util
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char

testInput = [(0,3),(1,2),(4,4),(6,4)]

p13 = do
  input <- lines <$> slurp 13
  let parsed = rights $ map (runParser parseFirewall "") input
      naiveCost = sum $ map fst $ filter snd $ map collision parsed
  print naiveCost

collision (d,1) = (d, True)
collision (d,2) = (d * 2, 0 == mod d 2)
collision (d,r) = (d * r, 0 == mod d (r + (r - 2)))

parseFirewall :: Parser (Int, Int)
parseFirewall = do
  depth <- num
  string ": "
  range <- num
  return (depth, range)
