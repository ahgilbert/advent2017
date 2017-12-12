module P12 where

import Util
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

p12 = do
  input <- lines <$> slurp 12
  print "ok"

parseNode :: Parser [(Int, Int)]
parseNode = do
  node <- num
  string " <-> "
  connected <- sepBy num (string ", ")
  return $ map (\c -> (node, c)) connected
