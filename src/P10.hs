module P10 where

import Util
import Data.List
import Data.List.Split

-- 1560 is too high
p10size = 256

p10 = do
  input <- map read <$> splitOn "," <$> slurp 10 :: IO [Int]
  let initList = take p10size [0..]
      allp10 = scanl step (initList, 0, 0) input
      p10_1 = p10hash $ (\(x,_,_) -> x) $ last allp10
  mapM_ print $ map (\(x,_,_) -> x) allp10
  print p10_1

step :: ([a], Int, Int) -> Int -> ([a], Int, Int)
step (list, start, skip) n =
  let list' = drop start $ cycle list
      twist = reverse $ take n list'
      remainder = drop n list'
      newList = take p10size (twist ++ remainder)
      newStart = mod (start + n + skip) p10size
  in (newList, newStart, skip + 1)

p10hash ls =
  (head ls) * (head $ tail ls)
