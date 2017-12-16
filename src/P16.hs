module P16 where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char

data DanceMove = Spin Int | Exchange Int Int | Partner Char Char

p16 = do
  print "ok"


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
