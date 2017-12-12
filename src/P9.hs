module P9 where

import Util
import Data.Either
import Data.Maybe
import Data.Tree
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data P9Stream = Garbage String | Group [P9Stream]

p9 = do
  input <- slurp 9
  let parsed = runParser parseGroup "" input
  print $ either (\_ -> -1) score1 parsed
  print $ either (\_ -> -1) score2 parsed

-- unfoldTree :: (b -> (a, [b]) -> b -> Tree a
-- g -> ((), [groupsOnly])
faith :: P9Stream -> (Int, [P9Stream])
faith (Garbage s) = (0,[])
faith (Group gs) = (1,gs)

hope (Garbage s) = (length s, [])
hope (Group gs) = (0,gs)

score1 :: P9Stream -> Int
score1 (Garbage _) = 0
score1 s =
  let tree = unfoldTree faith s
      byDepth = zip [1..] $ levels tree
  in sum $ concatMap (\(d,es) -> map (d *) es) byDepth

score2 s =
  let tree = unfoldTree hope s
  in sum $ flatten tree

parseGroup :: Parser P9Stream
parseGroup = do
  char '{'
  contents <- sepBy (parseGroup <|> parseGarbage) (char ',')
  char '}'
  return $ Group contents

parseGarbage :: Parser P9Stream
parseGarbage = do
  char '<'
  contents <- many p9char
  char '>'
  return $ Garbage (concat contents)

p9char = escapedChar <|> unescapedChar

unescapedChar :: Parser String
unescapedChar = do
  c <- noneOf "!>"
  return [c]

escapedChar :: Parser String
escapedChar = do
  char '!'
  anyChar
  return []

