module Environments.Sokoban.Sokoban where

import           Data.Map (Map)
import qualified Data.Map as Map



data Tile = Clear
          | Box Object

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


data World = World
    { coordMap :: Map Coord Tile
    , sokoban  :: Coord
    , goals    :: [Coord]
    }

data Direction = LeftDir
               | RightDir
               | UpDir
               | DownDir

moveCreate :: Object -> Coord -> Coord -> Map Coord Tile -> Map Coord Tile
moveCreate name from to cMap =
  Map.insert to (Box name) (Map.insert from Clear cMap)

moveVector :: World -> Coord -> Maybe World
moveVector world vec  =
  let sokoPos = sokoban world
      moveTo = vec + sokoPos
      pushTo = (vec * 2) + sokoPos
      atTo = Map.lookup moveTo $ coordMap world
      atPushTo = Map.lookup pushTo $ coordMap world
  in case (atTo, atPushTo) of
       (Just Clear, _) -> Just $ world { sokoban = moveTo }
       (Just (Box n), Just Clear) ->
        Just $ world  { sokoban = moveTo
                      , coordMap = moveCreate n moveTo pushTo (coordMap world)
                      }
       (Nothing, _) -> Nothing
       (Just (Box _), _) -> Nothing

move :: World -> Direction -> Maybe World
move w UpDir    = moveVector w (Coord ( 0,  1))
move w DownDir  = moveVector w (Coord ( 0, -1))
move w LeftDir  = moveVector w (Coord (-1,  0))
move w RightDir = moveVector w (Coord ( 1,  0))

isSolved :: World -> Bool
isSolved = undefined
