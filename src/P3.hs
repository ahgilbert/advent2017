module P3 where

import Util
import Data.List

manhattan (a,b) (c,d) =
  let dx = max a c - min a c
      dy = max b d - min b d
  in dx + dy

getCoords n =
  -- given spiral pattern, and setting 1 to (0,0), get (x,y) of n
  let lowRights = map (\x -> x * x) $ map (\x -> x * 2 + 1) $ [0..]
      diagonalLength = length $ tail $ takeWhile (<= n) $ lowRights
      start = (diagonalLength, -diagonalLength)
      sideLength = 1 + (2 * (diagonalLength + 1))
  in (0,0)

--   76543
--   85432
--   96121
--   07890
--   12345

p3_1 = do
  input <- read <$> getInput 3
  print $ input + 0
