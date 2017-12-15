module P15 where

import Util
import Data.Char
import Numeric

-- input A = seed: 703, factor: 16807
-- input B = seed: 516, factor: 48271

genA = mkGenerator 16807
genB = mkGenerator 48271

seqA1 = getSeq genA 703
seqB1 = getSeq genB 516

seqA2 = filter (\i -> mod i 4 == 0) $ getSeq genA 703
seqB2 = filter (\i -> mod i 8 == 0) $ getSeq genB 516

getSeq gen seed = tail $ iterate gen seed

mkGenerator factor =
  (\i -> rem (i * factor) 2147483647)

bitsMatch n (a,b) =
  let trim cs = reverse $ take n $ reverse cs
      a' = trim $ printBin a
      b' = trim $ printBin b
  in a' == b'

printBin n = showIntAtBase 2 intToDigit n ""

p15_1 =
  let sa = seqA1
      sb = seqB1
      numMatches = foldl faith 0 $ take 40000000 $ zip sa sb
  in print $ numMatches

p15_2 =
  let sa = seqA2
      sb = seqB2
      numMatches = foldl faith 0 $ take 5000000 $ zip sa sb
  in print $ numMatches

faith :: Int -> (Int, Int) -> Int
faith acc (a,b) =
  if bitsMatch 16 (a,b)
  then acc + 1
  else acc
