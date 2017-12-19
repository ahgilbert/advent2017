module P18 where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char

p18 =
  print $ "ok"

data Duet =
  Sound Val
  | Set Register Val
  | Add Register Val
  | Mul Register Val
  | Mod Register Val
  | Rcv Register
  | Jump Val Val
type Register = Char
data Val = Const Int | Reg Register
