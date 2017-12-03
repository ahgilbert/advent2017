module P2 where

import Data.List
import Util

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
