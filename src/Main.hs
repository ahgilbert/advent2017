{-# LANGUAGE ScopedTypeVariables #-}

module Main where

main :: IO ()
main = p1_2

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


getPair :: [Int] -> (Int, Int)
getPair xs = (maximum xs, minimum xs)

p2_1 =
  (map words . lines) <$> getInput 2
  -- >>= (\xs -> return $ map (getPair . map read) xs)
