module P16 where

import Util
import Data.Either
import Data.List
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

data DanceMove = Spin Int | Exchange Int Int | Partner Char Char

p16 = do
  input <- slurp 16
  let moves = head $ rights [runParser parseDanceMoves "" input]
      position1 = dance 1 "abcdefghijklmnop" moves
      position2 = dance' 2 "abcdefghijklmnop" moves
  print $ "after one dance: " ++ position1
  print $ "after two dances: " ++ position2

swap :: [a] -> Int -> Int -> [a]
swap lineup a b =
  let a' = min a b
      b' = max a b
      k = lineup !! a'
      j = lineup !! b'
  in (take a' lineup) ++ [j] ++ (drop (a' + 1) $ take b' lineup) ++ [k] ++ (drop (b' + 1) lineup)

dance 0 lineup _ =
  lineup
dance n lineup moves =
  let lineup' = foldl' danceMove lineup moves
  in dance (n - 1) lineup' moves

danceMove lineup (Spin n) =
  let idx = (length lineup) - n
      (a,b) = splitAt idx lineup
  in b ++ a
danceMove lineup (Exchange a b) =
  swap lineup a b
danceMove lineup (Partner a b) =
  let a' = fromJust $ elemIndex a lineup
      b' = fromJust $ elemIndex b lineup
  in swap lineup a' b'

parseDanceMoves :: Parser [DanceMove]
parseDanceMoves = sepBy (parseSpin <|> parseExchange <|> parsePartner) (string ",")

parseSpin = do
  char 's'
  size <- num
  return $ Spin size

parseExchange = do
  char 'x'
  a <- num
  char '/'
  b <- num
  return $ Exchange a b

parsePartner = do
  char 'p'
  a <- letterChar
  char '/'
  b <- letterChar
  return $ Partner a b
