module P18 where

import Util
import Control.Monad.State.Lazy
import Data.Array.IO
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char

type DuetState = StateT (Int, DuetArray) IO
type DuetArray = IOArray Int Duet
data Duet =
  Sound Register
  | Set Register Val
  | Add Register Val
  | Mul Register Val
  | Mod Register Val
  | Rcv Register
  | Jump Val Val
  deriving (Show)
type Register = Char
data Val = Const Int | Reg Register
  deriving Show

p18 = do
  input <- lines <$> slurp 18
  let parsed = rights $ map (runParser parseDuet "") input
      numCmds = length parsed
  arr <- newArray (0, numCmds) (Sound 'a') :: IO DuetArray
  mapM_ (\(l,d) -> writeArray arr l d) $ zip [0..numCmds] parsed
  print $ numCmds

duetStep :: DuetState Int
duetStep = do
  (pc, arr) <- get
  cmd <- liftIO $ readArray arr pc
  let pc' = execDuet arr pc
  put (pc', arr)
  return pc'

execDuet :: DuetArray -> Int -> Int
execDuet = undefined

--------- parsers -----------

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
