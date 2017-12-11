module P7 where

import Util
import Control.Monad.Reader
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
  faith <- runReaderT (unfoldTreeM genTower root) parsed
  print $ length $ levels faith

matchKey :: String -> Circuit -> Bool
matchKey t (k,_,_) = k == t

genTower :: String -> ReaderT [Circuit] IO (Circuit, [String])
genTower root = do
  circuits <- ask
  let c@(_,_,cs) = fromJust $ find (matchKey root) circuits
  return (c, cs)

parseCircuit :: Parser Circuit
parseCircuit = do
  key <- some letterChar
  space
  weight <- read <$> between (char '(') (char ')') (some digitChar)
  kids <- optional $ space >> string "->" >> space >> sepBy (some letterChar) (string ", ")
  return (key, weight, fromMaybe [] kids)
