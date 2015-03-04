module Environment.Sokoban.Samples.LargeSample (world) where

import Environment.Sokoban hiding (goals)
p = Right True
e = Right False
c = Left $ const Clear
b = Left Box

worldmap :: [[Either (String -> Tile) Bool]]
worldmap = [ [p, b, c, c]
           , [b]
           , [c]
           , [c]
           ]

goals :: [Coord]
goals = [ Coord (0, 3)
        , Coord (3, 0)
        ]

world :: World
world = from2DList worldmap goals
