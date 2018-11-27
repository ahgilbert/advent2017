module P18 where

import Util
import Control.Monad.State.Lazy
import Data.Array.IO
import Data.List
import Data.Either
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

type DuetState = StateT DuetMachine IO
data DuetMachine = DuetMachine {
  pc :: Int,
  lastSound :: Maybe Int,
  commands :: DuetArray,
  registers :: M.Map Char Int
  }
type DuetArray = IOArray Int Duet
data Duet =
    Nil
  | Sound Val
  | Set Register Val
  | Add Register Val
  | Mul Register Val
  | Mod Register Val
  | Rcv Val
  | Jump Val Val
  deriving (Show)
type Register = Char
data Val = Const Int | Reg Register
  deriving Show
data Faith a = Go Int | Terminate a

p18 = do
  input <- lines <$> slurp 18
  let parsed = rights $ map (runParser parseDuet "") input
      numCmds = length parsed
      registers = M.fromList $ map (\x -> (x,0)) $ nub $ mapMaybe getRegister parsed
  putStrLn $ (show numCmds) <> " out of " <> (show $ length input) <> " commands parsed" -- TODO execute machine
  arr <- newArray (0, numCmds) Nil :: IO DuetArray
  mapM_ (\(l,d) -> writeArray arr l d) $ zip [0..numCmds] parsed
  (finalValue, finalState) <- runStateT duetLoop (initVal arr registers)
  print finalValue

duetLoop :: DuetState Int -- StateT DuetMachine IO Int
duetLoop = do
  inc <- duetStep
  case inc of
    (Terminate n) -> return n
    (Go n) -> do
      s <- get
      put (s { pc = (pc s) + n })
      duetLoop

duetStep :: DuetState (Faith Int)
duetStep = do
  s <- get
  -- liftIO $ showState s
  cmd <- liftIO $ readArray (commands s) (pc s)
  exec cmd

showState :: DuetMachine -> IO ()
showState s =
  let regs = registers s
  in do
    cmd <- readArray (commands s) (pc s)
    mapM_ print regs
    print cmd

duetVal :: Val -> DuetState Int
duetVal (Reg r) = do
  registers <- registers <$> get
  return $ fromJust $ M.lookup r registers
duetVal (Const i) =
  return i

--------- virtual machine ------------

exec :: Duet -> DuetState (Faith Int)
exec Nil = undefined -- something went wrong
exec (Sound r) = do -- Set lastSound to val of r
  v <- duetVal r
  s <- get
  put (s { lastSound = (Just v) })
  return (Go 1)
exec (Set r v) = do -- set r to v
  v <- duetVal v
  s <- get
  let newMap = M.insert r v (registers s)
  put (s { registers = newMap })
  return (Go 1)
exec (Add r v) = do -- set r to r + v
  v <- duetVal v
  s <- get
  let newMap = M.insertWith (+) r v (registers s)
  put (s { registers = newMap })
  return (Go 1)
exec (Mul r v) = do -- set r to r * v
  v <- duetVal v
  s <- get
  let newMap = M.insertWith (*) r v (registers s)
  put (s { registers = newMap })
  return (Go 1)
exec (Mod r v) = do -- set r to r % v
  v <- duetVal v
  s <- get
  let newMap = M.insertWith (flip mod) r v (registers s)
  put (s { registers = newMap })
  return (Go 1)
exec (Rcv r) = do -- if r is not 0, return last sound played
  v <- duetVal r
  if v == 0
  then return (Go 1)
  else do
    s <- get
    return (Terminate (fromJust $ lastSound s))
exec (Jump r v) = do -- if r is greater than 0, skip v commands
  r' <- duetVal r
  v' <- duetVal v
  if r' <= 0
  then return (Go 1)
  else return (Go v')

getRegister :: Duet -> Maybe Register
getRegister (Set a _) = Just a
getRegister (Add a _) = Just a
getRegister (Mul a _) = Just a
getRegister (Mod a _) = Just a
getRegister _ = Nothing

initVal arr regs = DuetMachine { pc = 0, lastSound = Nothing, commands = arr, registers = regs }

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

parseMono :: String -> Parser Val
parseMono s = do
  string s
  space
  reg <- letterChar
  return (Reg reg)

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
