module P17 where

import Util
import Data.Array.IO
import Data.List
import Data.List.Split

p17input = 304

spinOnce (ls, pos, step) =
  let
    size = length ls
    newPos = (mod (pos + p17input) size) + 1
    (prefix, suffix) = splitAt newPos ls
    newLs = prefix ++ (step : suffix)
  in (newLs, newPos, step + 1)

p17 = do
  let spins = iterate spinOnce ([0], 0, 1)
      interesting = spins !! 2017
      p1 = take 2 $ dropWhile (/= 2017) $ (\(ls,_,_) -> ls) interesting
  print p1
