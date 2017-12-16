module P16 where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char

data DanceMove = Spin Int | Exchange Int Int | Partner Char Char

p16 = do
  print "ok"


parseDanceMoves :: Parser [DanceMove]
parseDanceMoves = sepBy (parseSpin <|> parseExchange <|> parsePartner) (string ",")

parseSpin = undefined

parseExchange = undefined

parsePartner = undefined
