module Environment.Sokoban.Samples.BigSample (world) where

import Environment.Sokoban hiding (goals)

p, e, c, b :: Either (Object -> Tile) Bool
p = Right True
e = Right False
c = Left $ const Clear
b = Left Box

worldmap :: [[Either (String -> Tile) Bool]]
worldmap = [ [c, c, e, e, e, e]
           , [c, b, c, c, c, c]
           , [c, c, c, c, b, p]
           , [e, e, c, c, e, e]
           ]
-- worldmap = [ [e, e, e, e, e, e]
--            , [e, e, c, c, c, e]
--            , [e, e, c, c, b, p]
--            , [e, e, c, c, e, e]
--            ]

goals :: [Coord]
goals = [
          Coord (1, 2),
          Coord (4, 1)
        --  Coord (2, 2)
        ]

world :: World
world = from2DList worldmap goals
