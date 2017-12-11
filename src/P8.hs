module P8 where

import Data.Void
import Text.Megaparsec

type Instruction = (Register, Cmd, Condition)
type Register = String
data Cmd = Inc Int | Dec Int
data Condition = Cond Register Comparison Int
data Comparison = Eq | Neq | GT | GTE | LT | LTE
type Parser = Parsec Void String

p8 = do
  print "ok"

-- ioe dec 890 if qk > -10
-- gif inc -533 if qt <= 7
-- itw dec 894 if t != 0
-- nwe inc 486 if hfh < -2
parse :: Parser Instruction
parse = undefined
