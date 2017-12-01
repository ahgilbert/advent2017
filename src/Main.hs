module Main where

main :: IO ()
main = p1_1

p1_1 :: IO ()
p1_1 = do
  raw <- readFile "input/1.txt"
  let raw' = takeWhile (\x -> elem x "0123456789") raw
  let closedLoop = [(head raw', last raw')]
  let pairs = (zip raw' $ tail raw') ++ closedLoop
  let pairs' = filter (\(x,y) -> x == y) pairs
  let total = sum $ map (\(x,_) -> read [x]) pairs'
  print $ total
