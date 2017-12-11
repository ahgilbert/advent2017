module P6 where

import Util
import Data.List
import Data.List.Split

p6 = do
  seed <- map read <$> splitOn "\t" <$> getInput 1006
  let redistribution = iterate step seed
  print $ step $ step seed

getDistribution ns =
  let biggest = maximum ns
      idx = length $ takeWhile (\x -> x /= biggest) ns
      (q,r) = quotRem biggest $ length ns
      toAdd = (replicate r (q+1)) ++ (replicate ((length ns) - r) q)
      shifted = rotate (idx + 1) toAdd
      emptied = set ns idx 0
  in (emptied, shifted)

-- from https://stackoverflow.com/questions/15530511/
set ns idx v =
  take idx ns ++ [v] ++ drop (idx + 1) ns

-- move the start of ns to the nth index
-- modified from https://stackoverflow.com/questions/16378773/
rotate _ [] = []
rotate n ns =
  let sn = reverse ns
  in reverse $ zipWith const (drop n (cycle sn)) sn

step ns =
  let (emptied, toAdd) = getDistribution ns
  in zipWith (+) emptied toAdd
