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

parseNum :: Parser Int
parseNum = do
  sign <- optional $ char '-'
  num <- some digitChar
  let n = read num :: Int
  if (isJust sign)
    then return ((-1) * n)
    else return n

