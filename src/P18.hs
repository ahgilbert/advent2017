module P18 where

import Util
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char

p18 = do
  input <- lines <$> slurp 18
  let parsed = rights $ map (runParser parseDuet "") input
  print $ length parsed

data Duet =
  Sound Register
  | Set Register Val
  | Add Register Val
  | Mul Register Val
  | Mod Register Val
  | Rcv Register
  | Jump Val Val
type Register = Char
data Val = Const Int | Reg Register

parseDuet :: Parser Duet
parseDuet =
  parseSound <|>
  parseSet <|>
  parseAdd <|>
  parseMul <|>
  parseMod <|>
  parseRcv <|>
  parseJump

parseConst :: Parser Val
parseConst = do
  n <- num
  return $ Const n

parseReg :: Parser Val
parseReg = do
  c <- letterChar
  return $ Reg c

parseVal :: Parser Val
parseVal =
  parseConst <|> parseReg

parseRegVal :: String -> Parser (Register, Val)
parseRegVal s = do
  string s
  space
  reg <- letterChar
  space
  v <- parseVal
  return (reg, v)

parseSound = do
  string "snd"
  space
  reg <- letterChar
  return $ Sound reg

parseSet = do
  (reg, v) <- parseRegVal "set"
  return $ Set reg v

parseAdd = do
  (reg, v) <- parseRegVal "add"
  return $ Add reg v

parseMul = do
  (reg, v) <- parseRegVal "mul"
  return $ Mul reg v

parseMod = do
  (reg, v) <- parseRegVal "mod"
  return $ Mod reg v

parseRcv = do
  reg <- letterChar
  return $ Rcv reg

parseJump = do
  string "jgz"
  space
  a <- parseVal
  space
  b <- parseVal
  return $ Jump a b
