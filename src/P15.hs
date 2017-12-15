module P15 where

import Util
import Data.Char
import Numeric

-- input A = seed: 703, factor: 16807
-- input B = seed: 516, factor: 48271

genA = mkGenerator 16807
genB = mkGenerator 48271

seqA = getSeq genA 703
seqB = getSeq genB 516

getSeq gen seed = tail $ iterate gen seed

mkGenerator factor =
  (\i -> rem (i * factor) 2147483647)

bitsMatch n (a,b) =
  let trim cs = reverse $ take n $ reverse cs
      a' = trim $ printBin a
      b' = trim $ printBin b
  in a' == b'

printBin n = showIntAtBase 2 intToDigit n ""

p15 = print "ok"
