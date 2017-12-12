module P10 where

import Util
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

p10 =
  print "ok"

parseNode :: Parser [(String, String)]
parseNode = do
  
