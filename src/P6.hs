module P6 where

import Util
import Data.List
import Data.List.Split

p6 = do
  seed <- map read <$> splitOn "\t" <$> getInput 6
  let annotated = markDupes $ iterate step seed
      untilDupe = takeWhile (\(x,_) -> not x) annotated
      firstDupe = find (\(x,_) -> x) annotated
      part1 = length untilDupe
  print $ "first duplicate appears after " ++ show part1 ++ " steps"

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

markDupes ns =
  map (\idx ->
         let n = ns !! idx
         in
          if elem n (take idx ns)
          then (True, n)
          else (False, n))
      [0..]
