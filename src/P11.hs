module P11 where

import Util
import Data.List.Split
import Math.Geometry.Grid
import Math.Geometry.Grid.Hexagonal2

p11 = do
  input <- splitOn "," <$> getInput 11
  let grid = UnboundedHexGrid
      dist = distance grid (0,0) (-1,1)
  print dist

move (x,y) "n"  = (x,y+1)
move (x,y) "ne" = (x+1,y)
move (x,y) "se" = (x+1,y-1)
move (x,y) "s"  = (x,y-1)
move (x,y) "sw" = (x-1,y)
move (x,y) "nw" = (x-1,y+1)
move _ _ = undefined
