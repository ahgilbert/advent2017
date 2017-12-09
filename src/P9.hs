module P9 where

import Util
import Data.Either
import Data.Maybe
import Data.Tree
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
data P9Stream = Garbage | Group [P9Stream]

p9_1 = do
  input <- Util.getInput 1009
  let parsed = runParser parseGroup "" input
  print $ either (\_ -> -1) score parsed

score :: P9Stream -> Int
score Garbage = 0
score (Group contents) = 1

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

