module P13 where

import Util
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char

testInput = [(0,3),(1,2),(4,4),(6,4)]

p13 = do
  input <- lines <$> slurp 13
  let parsed = rights $ map (runParser parseFirewall "") input
      naiveCost = tripCost parsed 0
      allTripCosts = map (tripCost parsed) [0..]
      bestDelay = length $ takeWhile (\x -> length x > 0) allTripCosts
  print naiveCost
  print bestDelay -- 1968 is too low

tripCost guards delay =
  map fst $ filter snd $ map (collision' delay) guards

collision' _ (d,1) = (d, True)
collision' delay (d,r) = (d, 0 == mod (d + delay) (2 * (r - 1)))

parseFirewall :: Parser (Int, Int)
parseFirewall = do
  depth <- num
  string ": "
  range <- num
  return (depth, range)
