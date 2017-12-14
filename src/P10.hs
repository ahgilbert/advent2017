module P10 where

import Util
import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import Numeric (showHex)
import System.IO.Unsafe

p10size = 256

p10 = do
  input <- map read <$> splitOn "," <$> slurp 10 :: IO [Int]
  let initList = take p10size [0..]
      step1 = knotHash input
      p10_1 = p1hash $ (\(x,_,_) -> x) $ step1
  print p10_1

knotHash input = foldl step ([0..p10size-1], 0, 0) input

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

p1hash ls =
  (head ls) * (head $ tail ls)

p10_2 = do
  sparse <- (\(x,_,_) -> x) <$> knotHash <$> concat <$> replicate 64 <$> p2input
  print $ sparseToDense sparse

p2input = map (ord) <$> (\cs -> cs ++ "17,31,73,47,23") <$> slurp 10

sparseToDense sparse =
  let bits = chunksOf 16 sparse
      xordBits = map (foldl xor 0) bits
      hexHash = map (\i -> showHex i "") xordBits
  in concat hexHash
