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

type Circuit = (String, Int, [String])

p7 = do
  input <- lines <$> Util.getInput 7
  let parsed = rights $ map (runParser parseCircuit "") input
      keys = map (\(x,_,_) -> x) parsed
      descendants = unique $ concatMap (\(_,_,x) -> x) parsed
      root = head $ filter (\k -> not $ elem k descendants) keys
  faith <- runReaderT (unfoldTreeM genTower root) parsed
  let weights = fmap (\(k,_,_) -> (k, weight parsed k)) faith
  print $ drawTree $ fmap (\(k,w) -> k ++ " " ++ (show w)) weights

matchKey :: String -> Circuit -> Bool
matchKey t (k,_,_) = k == t

weight :: [Circuit] -> String -> Int
weight circuits key =
  let (_,w,cs) = fromJust $ find (matchKey key) circuits
  in sum $ w : map (weight circuits) cs

genTower :: String -> ReaderT [Circuit] IO (Circuit, [String])
genTower base = do
  circuits <- ask
  let c@(_,_,cs) = fromJust $ find (matchKey base) circuits
  return (c, cs)

parseCircuit :: Parser Circuit
parseCircuit = do
  key <- some letterChar
  space
  weight <- read <$> between (char '(') (char ')') (some digitChar)
  kids <- optional $ space >> string "->" >> space >> sepBy (some letterChar) (string ", ")
  return (key, weight, fromMaybe [] kids)
