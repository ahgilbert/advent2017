module P3 where

import Util
import Data.List
import Data.Array.IO

type SpiralArray = IOArray (Int, Int) Int

getLayerStart target =
  let ends = zip [0..] $ map (^ 2) [1,3..]
      starts = ((0,0), 1) : map (\(i,m) -> ((i + 1, -i), m + 1)) ends
      start = last $ takeWhile (\(_,m) -> m <= target) starts
  in start

getSideLength x = 2 * x

proceed dist (ix,iy) =
  let sideLength = getSideLength ix
      right = ix
      left = ix - sideLength
      bottom = iy - 1
      top = bottom + sideLength
  in
    if dist < sideLength
    then (right, iy + dist)
    else if dist < (2 * sideLength)
    then (right - (dist - sideLength) - 1, top)
    else if dist < (3 * sideLength)
    then (left, top - 1 - (dist - (2 * sideLength)))
    else (left + 1 + dist - (3 * sideLength), bottom)

p3_1 = do
  input <- read <$> slurp 3
  let layerStart = getLayerStart input
      remainingDistance = input - (snd layerStart)
  print $ manhattan (0,0) $ proceed remainingDistance (fst layerStart)
  print ""

add (a,b) (c,d) = (a+c, b+d)

neighborhood :: SpiralArray -> (Int, Int) -> IO [Int]
neighborhood array center =
  mapM (readArray array) (neighbors center)
  where
    neighbors center =
      let steps = [(x,y) | x <- [-1,0,1], y <- [-1,0,1]]
      in map (add center) steps

leftTurn (0,1) = (-1,0)
leftTurn (-1,0) = (0,-1)
leftTurn (0,-1) = (1,0)
leftTurn (1,0) = (0,1)
leftTurn _ = undefined

step arr (_, loc, step, sides) = do
  neighbors <- neighborhood arr loc
  let loc' = add loc step
      total = sum neighbors
  writeArray arr loc total
  if (head sides) == 1
  then return (total, loc', leftTurn step, tail sides)
  else return (total, loc', step, (head sides - 1) : tail sides)

-- |Repeatedly evaluates the second argument until the value satisfies
-- the given predicate, and returns a list of all values that satisfied the
-- predicate.  Discards the final one (which failed the predicate).
unfoldWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
unfoldWhileM p m = loop id
    where
        loop f = do
            x <- m
            if p x
                then loop (f . (x:))
                else return (f [])

-- | Analogue of @('Prelude.until')@
-- Yields the result of applying f until p holds.
iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v
    | p v       = return v
    | otherwise = f v >>= iterateUntilM p f

p3_2 = do
  input <- read <$> slurp 3
  let width = (\(_,n) -> (n + 1) * 2) $ getLayerStart input
  arr <- createArray width
  writeArray arr (0,0) 1
  val <- iterateUntilM (\(t,_,_,_) -> t > input) (step arr) (1, (0,0), (1,0), pathLengths)
  return ()
  where
    createArray n = do
      arr <- newArray ((-n, -n), (n, n)) 0 :: IO SpiralArray
      return arr
    pathLengths = concat $ map (\x -> [x,x]) [1..]
