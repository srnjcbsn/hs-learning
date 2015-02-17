module Environments.Sokoban.Sokoban where

import qualified Data.Map as Map
import Data.Map (Map)

data Tile = Clear
          | Box

type Object = String

type Coord = (Int, Int)

data World = World
    { coordMap :: Map Coord Tile
    , objMap   :: Map Object Coord
    , sokoban  :: Coord
    }

data Direction = Left
               | Right
               | Up
               | Down


move :: World -> Direction -> World
move = undefined
