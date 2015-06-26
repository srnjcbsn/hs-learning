module Environment.Sokoban.Samples.SimpleSampleV (world) where

import           Environment.Sokoban hiding (goals)

p, c, b :: Either (Object -> Tile) Bool
p = Right True
-- e = Right False
c = Left $ const Clear
b = Left Box

worldmap :: [[Either (String -> Tile) Bool]]
worldmap = [ [c]
           , [c]
           , [b]
           , [c]
           , [p]
           ]

goals :: [Coord]
goals = [ Coord (0, 0) ]

world :: World
world = from2DList worldmap goals
