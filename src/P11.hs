module P11 where

import Util
import Data.List.Split
import Math.Geometry.Grid
import Math.Geometry.Grid.Hexagonal2

p11 = do
  input <- splitOn "," <$> takeWhile (\c -> elem c "nsew,") <$> getInput 11
  let grid = UnboundedHexGrid
      path = map (distance grid (0,0)) $ scanl move (0,0) input
      finalDist = (last path)
      maxDist = maximum path
  print $ "final distance: " ++ show finalDist
  print $ "max distance: " ++ show maxDist

move (x,y) "n"  = (x,y+1)
move (x,y) "ne" = (x+1,y)
move (x,y) "se" = (x+1,y-1)
move (x,y) "s"  = (x,y-1)
move (x,y) "sw" = (x-1,y)
move (x,y) "nw" = (x-1,y+1)
move _ _ = undefined
