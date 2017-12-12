module P8 where

import Util
import Data.Either
import Data.Maybe
import Data.Void
import qualified Data.HashTable.IO as HT
import Text.Megaparsec
import Text.Megaparsec.Char

type Instruction = (Register, Cmd, Condition)
type Register = String
data Cmd = Inc Int | Dec Int
data Condition = Cond Register Predicate Int | Pass
type Predicate = Int -> Int -> Bool
type Machine = HT.BasicHashTable Register Int

p8 = do
  input <- lines <$> slurp 1008
  let parsed = rights $ map (runParser parseInstruction "") input
      registers = map (\(r,_,_) -> r) parsed
  ht <- HT.fromList (zip registers (repeat 0)) :: IO Machine
  print $ length input
  print $ length parsed

-- ioe dec 890 if qk > -10
-- gif inc -533 if qt <= 7
-- itw dec 894 if t != 0
-- nwe inc 486 if hfh < -2
parseInstruction :: Parser Instruction
parseInstruction = do
  dest <- some letterChar
  space
  cmd <- some letterChar
  space
  arg <- num
  cond <- fromMaybe Pass <$> optional parseCondition
  return ("a", Inc 1, Pass)

parseCondition :: Parser Condition
parseCondition = do
  string "if"
  space
  reg <- some letterChar
  space
  comp <- some $ oneOf "<>=!"
  space
  val <- num
  return $ Cond reg (matchComparator comp) val
  where
    matchComparator "==" = (==)
    matchComparator "!=" = (/=)
    matchComparator "<" = (<)
    matchComparator ">" = (>)
    matchComparator "<=" = (<=)
    matchComparator ">=" = (>=)
    matchComparator _ = undefined
