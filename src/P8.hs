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
type Cmd = Int -> Int
data Condition = Cond Register Predicate Int | Pass
type Predicate = Int -> Int -> Bool
type Machine = HT.BasicHashTable Register Int

p8 = do
  input <- lines <$> slurp 8
  let parsed = rights $ map (runParser parseInstruction "") input
      registers = map (\(r,_,_) -> r) parsed
  dict <- createMap registers
  mapM_ (evalInstruction dict) parsed
  vals <- map snd <$> HT.toList dict
  print $ maximum vals

createMap keys = do
  dict <- HT.fromList $ zip keys (repeat 0) :: IO (HT.BasicHashTable String Int)
  return dict

evalInstruction :: Machine -> Instruction -> IO ()
evalInstruction dict (reg, cmd, cond) = do
  continue <- evalCond dict cond
  if continue
  then do
    print $ reg ++ " passes comparison"
    currentVal <- fromJust <$> HT.lookup dict reg
    let newVal = cmd currentVal
    HT.insert dict reg newVal
  else return ()

evalCond :: Machine -> Condition -> IO Bool
evalCond _ Pass = do
  print "eval pass condition"
  return True
evalCond dict (Cond reg cmp v) = do
  print $ "eval condition on " ++ reg
  regVal <- fromJust <$> HT.lookup dict reg
  return $ cmp regVal v

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
  let changer =
        if cmd == "inc"
        then (\n -> n + arg)
        else (\n -> n - arg)
  return (dest, changer, cond)

parseCondition :: Parser Condition
parseCondition = do
  space
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
