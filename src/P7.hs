module P7 where

import Util
import Control.Monad.State as S
import Data.Either
import Data.List
import Data.List.Unique
import Data.Maybe
import Data.Tree
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type Circuit = (String, Int, [String])

p7 = do
  input <- lines <$> Util.getInput 1007
  let parsed = rights $ map (runParser parseCircuit "") input
      keys = map (\(x,_,_) -> x) parsed
      descendants = unique $ concatMap (\(_,_,x) -> x) parsed
      root = head $ filter (\k -> not $ elem k descendants) keys
  -- put parsed
  -- unfoldTreeM genTowerM root
  print $ "ok"

matchKey :: String -> Circuit -> Bool
matchKey t (k,_,_) = k == t

genTowerM :: String -> S.State [Circuit] (Circuit, [String])
genTowerM =
  undefined

parseCircuit :: Parser Circuit
parseCircuit = do
  key <- some letterChar
  space
  weight <- read <$> between (char '(') (char ')') (some digitChar)
  kids <- optional $ space >> string "->" >> space >> sepBy (some letterChar) (string ", ")
  return (key, weight, fromMaybe [] kids)
