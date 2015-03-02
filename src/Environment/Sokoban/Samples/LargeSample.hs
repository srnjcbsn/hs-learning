module Environment.Sokoban.Samples.LargeSample (world) where

import Environment.Sokoban hiding (goals)
p = Right True
e = Right False
c = Left $ const Clear
b = Left Box

worldmap :: [[Either (String -> Tile) Bool]]
worldmap = [ [p, b, c]
           , [b]
           , [c]
           ]

goals :: [Coord]
goals = [ Coord (0, 2)
        , Coord (2, 0)
        ]

world :: World
world = from2DList worldmap goals
