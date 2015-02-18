module Environments.Sokoban.PDDL where

import           Environments.Sokoban.Sokoban hiding (Object)
import           PDDL.Type

import           Data.Char                    (isDigit)
import           Data.List                    (partition)
import           Data.Map                     (Map, (!))
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import Data.Maybe (mapMaybe)

data SokobanPDDL = SokobanPDDL
    { world           :: World
    , locMap          :: Map Location Coord
    , persistentState :: Set GroundedPredicate
    }

data Adj = Adj (Object, Object) deriving Eq
type Structure = ([Adj], [Adj])

type Location = Object
type Crate = Object

hAdjName, vAdjName :: String
hAdjName = "hAdj"
vAdjName = "vAdj"

-- hAdj :: Location -> Location -> GroundedPredicate
-- hAdj from to = ("hAdj", [from, to])
--
-- vAdj :: Location -> Location -> GroundedPredicate
-- vAdj from to = ("vAdj", [from, to])

sokobanAt :: Location -> GroundedPredicate
sokobanAt loc = ("sokobanAt", [loc])

at :: Crate -> Location -> GroundedPredicate
at crate loc = ("at", [crate, loc])

atGoal :: Crate -> GroundedPredicate
atGoal crate = ("atGoal", [crate])

clear :: Location -> GroundedPredicate
clear loc = ("clear", [loc])

goal :: Location -> GroundedPredicate
goal loc = ("goal", [loc])

notGoal :: Location -> GroundedPredicate
notGoal loc = ("notGoal", [loc])

directionFromObjs :: SokobanPDDL -> Location -> Location -> Direction
directionFromObjs pddlw from to =
  let fromPos = locMap pddlw ! from
      toPos = locMap pddlw ! to
   in case toPos - fromPos of
        Coord (0, 1) -> UpDir
        Coord (0, -1) -> DownDir
        Coord (1, 0) -> RightDir
        Coord (-1, 0) -> LeftDir
        _ -> error ("cannot get direction (fromPos: " ++ show fromPos ++", toPos: " ++ show toPos ++ ")")

applyFromLoc :: SokobanPDDL -> Location -> Location -> SokobanPDDL
applyFromLoc pddlw from to = pddlw { world = move (world pddlw) $ directionFromObjs pddlw from to}

applyAction :: SokobanPDDL -> Action -> SokobanPDDL
applyAction pddlw ("move-h", [from, to]) = applyFromLoc pddlw from to
applyAction pddlw ("move-v", [from, to]) = applyFromLoc pddlw from to
applyAction pddlw ("push-h", [_, from, to, _]) = applyFromLoc pddlw from to
applyAction pddlw ("push-v", [_, from, to, _]) = applyFromLoc pddlw from to
applyAction pddlw ("push-h-goal", [_, from, to, _]) = applyFromLoc pddlw from to
applyAction pddlw ("push-v-goal", [_, from, to, _]) = applyFromLoc pddlw from to
applyAction _ act = error ("Unknown action: " ++ show act)

isStructurePred :: GroundedPredicate -> Bool
isStructurePred (name, _)
    | name == vAdjName = True
    | name == hAdjName = True
    | otherwise        = False

parseLocation :: Location -> Coord
parseLocation ('b' : n) = Coord (x, y) where
    (xStr, rest) = span isDigit n
    x = read xStr
    y = read $ tail rest
parseLocation n = error $ "Location " ++ n ++ " could not be parsed."

writeLocation :: Coord -> Location
writeLocation (Coord (x, y)) = ('b' : show x) ++ ('x' : show y)

statePred :: GroundedPredicate -> Maybe StatePred
statePred ("goal", [a]) = Just $ Goal a
statePred ("at", [c, l]) = Just $ CrateLoc c l
statePred ("sokobanAt", [l]) = Just $ SokobanLoc l
-- statePred (name, [a, b])
--     | name == hAdjName || name == vAdjName = Structure (min a b) (max a b)
statePred p = Nothing -- error $ "Could not understand predicate " ++ show p

data StatePred = Structure Location Location
               | CrateLoc Crate Location
               | SokobanLoc Location
               | Goal Location

fromState :: State -> SokobanPDDL
fromState state = pddl where
    (structPreds, rest) = partition isStructurePred $ Set.toList state

    goalPreds = [ gp | gp@("goal", _) <- rest ]
    notGoalPreds = [ ngp | ngp @("notGoal", _) <- rest ]

    coordMap = Map.fromList
             $ Set.toList
             $ Set.map (\n -> (n, parseLocation n))
             $ Set.unions
             $ map (\(_, args) -> Set.fromList args) structPreds

    dataList = mapMaybe statePred rest
    -- If this is an empty list, we throw an error, as we wouldn't be able
    -- to go on without knowing the location of Sokoban:
    sokoLoco = coordMap ! head [ l | SokobanLoc l <- dataList ]
    crates   = [ (coordMap ! l, Box c) | CrateLoc c l <- dataList ]
    goals    = [ coordMap ! g | Goal g <- dataList ]

    -- Set all tiles as 'Clear'
    clearTiles = Map.fromList $ zip (Map.elems coordMap) (repeat Clear)
    -- Overwrite the tiles that contain crates
    tileMap  = Map.fromList crates `Map.union` clearTiles

    world = World
        { coordMap = tileMap
        , sokoban  = sokoLoco
        , goals    = goals
        }

    pddl = SokobanPDDL
        { world = world
        , locMap = coordMap
        , persistentState = Set.fromList $ structPreds ++ goalPreds ++ notGoalPreds
        }

worldPreds :: World -> Set GroundedPredicate
worldPreds world = Set.insert sokLoc tilePreds where

    sokLoc = sokobanAt $ writeLocation $ sokoban world
    -- The tilemap minus the tile sokoban is standing on
    tileMap' = Map.delete (sokoban world) (coordMap world)
    tilePreds = Set.fromList $ map (uncurry tilePred) $ Map.toList tileMap'

    tilePred :: Coord -> Tile -> GroundedPredicate
    tilePred coord Clear = clear $ writeLocation coord
    tilePred coord (Box n)
        | coord `elem` goals world = atGoal n
        | otherwise = at n $ writeLocation coord

toState :: SokobanPDDL -> State
toState pddl =
     worldPreds (world pddl) `Set.union` persistentState pddl
