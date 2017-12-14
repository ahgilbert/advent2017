module P10 where

import Util
import Data.List
import Data.List.Split
import System.IO.Unsafe

p10size = 256

p10 = do
  input <- map read <$> splitOn "," <$> slurp 10 :: IO [Int]
  let initList = take p10size [0..]
      allp10 = scanl step (initList, 0, 0) input
      p10_1 = p10hash $ (\(x,_,_) -> x) $ last allp10
  mapM_ print $ allp10
  print p10_1

step :: ([a], Int, Int) -> Int -> ([a], Int, Int)
step (list, start, skip) n =
  let end = mod (start + n) p10size
      twist = reverse $ take n $ drop start $ cycle list
      newStart = mod (start + n + skip) p10size
      newList =
        if n == 0
        then list
        else
          if start < end -- we have not wrapped back to start
          then take p10size $ (take start list) ++ twist ++ (drop (start + n) list)
          else
            let breakPoint = p10size - start
                prefix = drop breakPoint twist
                suffix = take breakPoint twist
                unchanged = take (start - end) $ drop end list
            in prefix ++ unchanged ++ suffix
  in (newList, newStart, skip + 1)

p10hash ls =
  (head ls) * (head $ tail ls)
