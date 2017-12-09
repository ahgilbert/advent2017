module P7 where

import Util
import Data.Either
import Data.Maybe
import Data.Tree
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type Circuit = (String, Int, [String])

p7 = do
  input <- lines <$> Util.getInput 7
  let parsed = map (runParser parseCircuit "") input
  return ()

parseCircuit :: Parser Circuit
parseCircuit = do
  key <- some letterChar
  weight <- read <$> between (char '(') (char ')') (some digitChar)
  string " -> "
  kids <- sepBy (some letterChar) (string ", ")
  return (key, weight, kids)
