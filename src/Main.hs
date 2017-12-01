module Main where

main :: IO ()
main = p1_1

getInput1 = do
  raw <- readFile "input/1.txt"
  return $ takeWhile (\x -> elem x "0123456789") raw

offsetBy offset source =
  zip source $ (drop offset source) ++ (take offset source)

p1_1 :: IO ()
p1_1 = do
  raw <- getInput1
  let pairs = offsetBy 1 raw
  let pairs' = filter (\(x,y) -> x == y) pairs
  let total = sum $ map (\(x,_) -> read [x]) pairs'
  print $ total
