{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List

main :: IO ()
main = p2_2

getInput n = do
  let filename = "input/" ++ show n ++ ".txt"
  raw <- readFile filename
  return raw

offsetBy offset source =
  zip source $ (drop offset source) ++ (take offset source)

p1_1 :: IO ()
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

input2 :: IO [[Int]]
input2 = (map (map read . words)) . lines <$> getInput 2

p2_1 =
  map (\(h,l) -> h - l) <$> map (\xs -> (maximum xs, minimum xs)) <$> input2
  >>= (\ns -> return $ sum ns)
  >>= print

pairs :: [a] -> [(a,a)]
pairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

p2_2 = do
  matches <- sum
             <$> map (\(a,b) -> div a b)
             <$> filter (\(a,b) -> rem a b == 0)
             <$> concatMap pairs
             <$> map descending
             <$> input2
  print matches
  where
    descending = (\ns -> reverse . sort $ ns)
