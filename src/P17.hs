module P17 where

import Util
import Data.Array.IO
import Data.List
import Data.List.Split

p17input = 304

spinOnce (ls, pos, step) =
  let
    newPos = (mod (pos + p17input) step) + 1
    (prefix, suffix) = splitAt newPos ls
    newLs = prefix ++ (step : suffix)
  in (newLs, newPos, step + 1)

p17_1 = do
  let spins = iterate spinOnce ([0], 0, 1)
      interesting = spins !! 2017
      p1 = take 2 $ dropWhile (/= 2017) $ (\(ls,_,_) -> ls) interesting
  print p1

p17_2 = do
  let finalSpin = foldl' p2fold (0,0,0) [1..50000000]
  print $ (\(pos,_,best) -> best) finalSpin

p2fold :: (Int, Int, Int) -> Int -> (Int, Int, Int)
p2fold (currentPos, zeroIdx, best) step =
  let
    newPos = (mod (currentPos + p17input) step) + 1
  in
    if newPos < zeroIdx
    then (newPos, zeroIdx + 1, best)
    else
      if newPos == (zeroIdx + 1)
      then (newPos, zeroIdx, step)
      else (newPos, zeroIdx, best)

showResult17 target foldOutput =
  take 2 $ dropWhile (/= target) $ (\(x,_,_) -> x) foldOutput
