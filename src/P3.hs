module P3 where

import Util
import Data.List

getLayerStart target =
  let ends = zip [0..] $ map (^ 2) [1,3..]
      starts = ((0,0), 1) : map (\(i,m) -> ((i + 1, -i), m + 1)) ends
      start = last $ takeWhile (\(_,m) -> m <= target) starts
  in start

getSideLength x =
  2 * x

proceed dist (ix,iy) =
  let sideLength = getSideLength ix
      right = ix
      left = ix - sideLength
      bottom = iy - 1
      top = bottom + sideLength
  in
    if dist < sideLength
    then (right, iy + dist)
    else if dist < (2 * sideLength)
    then (right - (dist - sideLength) - 1, top)
    else if dist < (3 * sideLength)
    then (left, top - 1 - (dist - (2 * sideLength)))
    else (left + 1 + dist - (3 * sideLength), bottom)

p3_1 = do
  input <- read <$> getInput 3
  let layerStart = getLayerStart input
      remainingDistance = input - (snd layerStart)
  print $ manhattan (0,0) $ proceed remainingDistance (fst layerStart)
  print ""
