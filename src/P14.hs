module P14 where

import Util
import P10 (knotHash)
import Data.Char
import Data.Graph
import Data.List.Split
import Data.Maybe
import Numeric

p14input = "vbqugkhl"

p14strings =
  map longHexToBin $
  map knotHash $
  map (\i -> p14input ++ "-" ++ show i) [0..127]

p14_1 =
  print $
  sum $
  map (length . filter (== '1')) p14strings

p14_2 =
  print "ok"

longHexToBin s =
  concatMap hexToBin $ chunksOf 2 s

hexToBin s =
  let (num, _) = head $ readHex s
      str = pad 8 $ showIntAtBase 2 intToDigit num ""
  in str

{- for p2, I think I can use Data.Graph, giving 4 neighbors per node, but only creating nodes
   whose value is 1. Then I can reuse the `getGroups` function from P12

   "the out-list may contain keys that don't correspond to nodes
    of the graph; such edges are ignored" -}

testInput14 = ["0010101", -- should have 4 groups
               "0011101",
               "1000101",
               "1010001",
               "1010101",
               "0011101",
               "0010101"]

initGraph grid =
  let
      putItAllTogether = (\(rIdx,rowVals) ->
                            zipWith (\cIdx val -> (val, (cIdx,rIdx))) [0..] rowVals)
      taggedRows = zip [0..] grid
      taggedGrid = map putItAllTogether taggedRows
      nodes = concatMap (map (\(v,k) -> (v, k, neighbors k))) taggedGrid
      onNodes = filter (\(v,_,_) -> v == '1') nodes
      graph = graphFromEdges onNodes
  in graph
