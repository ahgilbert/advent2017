module P18 where

import Util
import Control.Monad.State.Lazy
import Data.Array.IO
import Data.Either
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

type DuetState = StateT DuetMachine IO
data DuetMachine = DuetMachine {
  pc :: Int,
  lastSound :: Int,
  commands :: DuetArray,
  registers :: M.Map Char Int
  }
type DuetArray = IOArray Int Duet
data Duet =
    Nil
  | Sound Register
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
  input <- lines <$> xslurp 18
  let parsed = rights $ map (runParser parseDuet "") input
      numCmds = length parsed
  putStrLn $ (show numCmds) <> " out of " <> (show $ length input) <> " commands parsed" -- TODO execute machine
  arr <- newArray (0, numCmds) Nil :: IO DuetArray
  mapM_ (\(l,d) -> writeArray arr l d) $ zip [0..numCmds] parsed

duetStep :: DuetState ()
duetStep = do
  machine <- get
  cmd <- liftIO $ readArray (commands machine) (pc machine)
  return ()

duetVal :: Val -> DuetState Int
duetVal (Reg r) = do
  registers <- registers <$> get
  return $ fromJust $ M.lookup r registers
duetVal (Const i) =
  return i

--------- virtual machine ------------

exec :: Duet -> DuetState ()
exec Nil = undefined -- something went wrong
exec (Sound r) = do -- Set lastSound to val of r
  v <- duetVal (Reg r)
  s <- get
  put (s { lastSound = v })
exec (Set r v) = do -- set r to v
  v <- duetVal v
  s <- get
  return ()
exec (Add r v) = undefined -- set r to r + v
exec (Mul r v) = undefined -- set r to r * v
exec (Mod r v) = undefined -- set r to r % v
exec (Rcv r) = undefined -- if r is not 0, return last sound played? Print it?
exec (Jump r v) = undefined -- if r is greater than 0, skip v commands

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

parseMono :: String -> Parser Register
parseMono s = do
  string s
  space
  reg <- letterChar
  return reg

parseSound = do
  reg <- parseMono "snd"
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
  reg <- parseMono "rcv"
  return $ Rcv reg

parseJump = do
  string "jgz"
  space
  a <- parseVal
  space
  b <- parseVal
  return $ Jump a b
