module P15 where

import Util
import Data.Char
import Numeric

genA = mkGenerator 703 16807
genB = mkGenerator 516 48271

mkGenerator seed factor =
  undefined

bitsMatch n (a,b) =
  let trim cs = reverse $ take n $ reverse cs
      a' = trim $ printBin a
      b' = trim $ printBin b
  in a' == b'

printBin n = showIntAtBase 2 intToDigit n ""

p15 = print "ok"
