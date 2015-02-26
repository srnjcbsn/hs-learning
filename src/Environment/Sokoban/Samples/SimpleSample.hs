module Environment.Sokoban.Samples.SimpleSample (world) where

import Environment.Sokoban hiding (goals)
p = Right True
e = Right False
c = Left $ const Clear
b = Left Box

worldmap :: [[Either (String -> Tile) Bool]]
worldmap = [ [c]
           , [p]
           ]

goals :: [Coord]
goals = [
        ]

world :: World
world = from2DList worldmap goals
