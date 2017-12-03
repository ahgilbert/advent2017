module P1 where

import Util

offsetBy offset source =
  zip source $ (drop offset source) ++ (take offset source)

p1_1 = do
  raw <- takeWhile (\c -> elem c "0123456789") <$> getInput 1
  let pairs = offsetBy 1 raw
  let pairs' = filter (\(x,y) -> x == y) pairs
  let total = sum $ map (\(x,_) -> read [x]) pairs'
  print $ total

p1_2 = do
  raw <- takeWhile (\c -> elem c "0123456789") <$> getInput 1
  let offset = div (length raw) 2
  let pairs = offsetBy offset raw
  let pairs' = filter (\(x,y) -> x == y) pairs
  let total = sum $ map (\(x,_) -> read [x]) pairs'
  print $ total
