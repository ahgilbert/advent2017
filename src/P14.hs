module P14 where

import Util
import P10 (knotHash)
import Data.Char
import Data.Graph
import Data.List.Split
import Numeric

p14input = "vbqugkhl"

p14seeds =
  map knotHash $
  map (\i -> p14input ++ "-" ++ show i) [0..127]

p14 =
  print $
  sum $
  map (length . filter (== '1') . longHexToBin) p14seeds

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

initGraph grid =
  undefined
