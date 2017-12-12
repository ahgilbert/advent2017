module P4 where

import Util
import Data.List
import Data.List.Unique

p4_1 =
  length <$> filter (allUnique . words) <$> lines <$> slurp 4
  >>= print

p4_2 =
  length <$> filter (allUnique . (map sort) . words) <$> lines <$> slurp 4
  >>= print
