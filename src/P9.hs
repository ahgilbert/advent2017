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
  input <- Util.getInput 9
  let parsed = runParser parseGroup "" input
  print $ either (\_ -> -1) score parsed

-- unfoldTree :: (b -> (a, [b]) -> b -> Tree a
-- g -> ((), [groupsOnly])
faith :: P9Stream -> (Int, [P9Stream])
faith Garbage = (0,[])
faith (Group gs) = (1,gs)

score :: P9Stream -> Int
score Garbage = 0
score s =
  let tree = unfoldTree faith s
      byDepth = zip [1..] $ levels tree
  in sum $ concatMap (\(d,es) -> map (d *) es) byDepth

parseGroup :: Parser P9Stream
parseGroup = do
  char '{'
  contents <- sepBy (parseGroup <|> parseGarbage) (char ',')
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
unescapedChar = noneOf "!>"

escapedChar :: Parser Char
escapedChar = do
  char '!'
  anyChar

