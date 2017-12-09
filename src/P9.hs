module P9 where

import Util
import Data.Either
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
data P9Stream = Garbage | Group [P9Stream]

p9_1 = do
  input <- Util.getInput 9
  print input

parseStream :: Parser P9Stream
parseStream = do
  sign <- optional $ char '-'
  num <- some digitChar
  let n = read num :: Int
  return Garbage

parseGroup :: Parser P9Stream
parseGroup = do
  char '{'
  contents <- many $ parseGroup <|> parseGarbage
  char '}'
  return $ Group contents

parseGarbage :: Parser P9Stream
parseGarbage = do
  char '<'
  many p9char
  char '>'
  return Garbage

p9char = escapedChar <|> unescapedChar

unescapedChar :: Parser Char
unescapedChar = noneOf "!<"

escapedChar :: Parser Char
escapedChar = do
  char '!'
  anyChar

