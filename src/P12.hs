module P12 where

import Util
import Data.Either
import Data.Graph
import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

p12 = do
  input <- lines <$> slurp 12
  let parsedInput = rights $ map (runParser parseNode "") input
      nodes = zipWith (\x (y,z) -> (x,y,z)) (repeat ()) parsedInput
      keys = map (\(k,_) -> k) parsedInput
      (graph, faith, get) = graphFromEdges nodes
      p1 = reachable graph (fromJust $ get 0)
      p2 = getGroups graph keys get
  print $ length p1
  print $ length p2

parseNode :: Parser (Int, [Int])
parseNode = do
  node <- num
  string " <-> "
  connected <- sepBy num (string ", ")
  return $ (node, connected)

getGroups _ [] _ = []
getGroups graph keys getter =
  let seed = head keys
      group = reachable graph (fromJust $ getter seed)
  in group : (getGroups graph (keys \\ group) getter)
