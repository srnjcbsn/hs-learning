module Environment.Sokoban where

import           Data.Map (Map, (!))
import qualified Data.Map as Map

data Tile = Clear
          | Box Object
          deriving Show

isClear :: Tile -> Bool
isClear Clear = True
isClear _     = False

type Object = String

data Coord = Coord (Integer, Integer)
             deriving (Ord, Eq, Show)

instance Num Coord where
  (+) (Coord (x1,y1)) (Coord (x2,y2)) = Coord (x1+x2, y1+y2)
  (*) (Coord (x1,y1)) (Coord (x2,y2)) = Coord (x1+x2, y1+y2)
  abs (Coord (x,y)) = Coord (abs x, abs y)
  signum (Coord (x,y)) = Coord (signum x, signum y)
  fromInteger i = Coord (i, i)
  (-) (Coord (x1,y1)) (Coord (x2,y2)) = Coord (x1-x2, y1-y2)

xCoord :: Coord -> Int
xCoord (Coord (x, _)) = fromIntegral x

yCoord :: Coord -> Int
yCoord (Coord (_, y)) = fromIntegral y

data World = World
    { coordMap :: Map Coord Tile
    , sokoban  :: Coord
    , goals    :: [Coord]
    } deriving Show

data Direction = LeftDir
               | RightDir
               | UpDir
               | DownDir

moveCreate :: Object -> Coord -> Coord -> Map Coord Tile -> Map Coord Tile
moveCreate name from to cMap =
  Map.insert to (Box name) (Map.insert from Clear cMap)

moveVector :: World -> Coord -> World
moveVector world vec  =
  let sokoPos = sokoban world
      moveTo = vec + sokoPos
      pushTo = (vec * 2) + sokoPos
      atTo = Map.lookup moveTo $ coordMap world
      atPushTo = Map.lookup pushTo $ coordMap world
  in case (atTo, atPushTo) of
       (Just Clear, _) ->world { sokoban = moveTo }
       (Just (Box n), Just Clear) ->
            world { sokoban = moveTo
                  , coordMap = moveCreate n moveTo pushTo (coordMap world)
                  }
       (Nothing, _) -> world
       (Just (Box _), _) -> world

move :: World -> Direction -> World
move w UpDir    = moveVector w (Coord ( 0,  1))
move w DownDir  = moveVector w (Coord ( 0, -1))
move w LeftDir  = moveVector w (Coord (-1,  0))
move w RightDir = moveVector w (Coord ( 1,  0))

-- | The goal is satisfied when all goal tiles are occupied by boxes
isSolved :: World -> Bool
isSolved world = all ((not . isClear) . (coordMap world !)) (goals world)
