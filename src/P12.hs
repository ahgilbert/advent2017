module P12 where

import Util
import Data.Either
import Data.Graph
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

p12 = do
  input <- lines <$> slurp 12
  let parsedInput = rights $ map (runParser parseNode "") input
      nodes = zipWith (\x (y,z) -> (x,y,z)) (repeat ()) parsedInput
      (graph, faith, get) = graphFromEdges nodes
      p1 = reachable graph (fromJust $ get 0)
  print $ length p1

parseNode :: Parser (Int, [Int])
parseNode = do
  node <- num
  string " <-> "
  connected <- sepBy num (string ", ")
  return $ (node, connected)
