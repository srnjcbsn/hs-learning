module Environment.Sokoban.Samples.WikiSample (world) where

import Environment.Sokoban hiding (goals)

p, e, c, b :: Either (Object -> Tile) Bool
p = Right True
e = Right False
c = Left $ const Clear
b = Left Box

worldmap :: [[Either (String -> Tile) Bool]]
worldmap = [ [e,e,e,e,e,e,e,e]
           , [e,e,e,c,c,c,e,e]
           , [e,c,p,b,c,c,e,e]
           , [e,e,e,c,b,c,e,e]
           , [e,c,e,e,b,c,e,e]
           , [e,c,e,c,c,c,e,e]
           , [e,b,c,b,b,b,c,e]
           , [e,c,c,c,c,c,c,e]
           , [e,e,e,e,e,e,e,e]
           ]

goals :: [Coord]
goals = [ Coord (1,2)
        , Coord (5,3)
        , Coord (1,4)
        , Coord (4,5)
        , Coord (6,6)
        , Coord (4,7)
        ]

world :: World
world = from2DList worldmap goals
