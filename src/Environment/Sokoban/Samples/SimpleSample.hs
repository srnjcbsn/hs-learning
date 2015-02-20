module Environment.Sokoban.Samples.SimpleSample (world) where

import Environment.Sokoban hiding (goals)
p = Right True
e = Right False
c = Left $ const Clear
b = Left Box

worldmap :: [[Either (String -> Tile) Bool]]
worldmap = [ [c, b, p]
           , [e, e, b]
           , [e, e, c]
           ]

goals :: [Coord]
goals = [ Coord (0, 0)
        , Coord (2, 2)
        ]

world :: World
world = from2DList worldmap goals
