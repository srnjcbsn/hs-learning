module Environments.Sokoban.PDDL where

import           Environments.Sokoban.Sokoban hiding (Object)
import           PDDL.Type

import           Data.Map                     (Map, (!))
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import Data.List (sort, partition)
import Data.Char (isDigit)

data SokobanPDDL = SokobanPDDL
    { world  :: World
    , objMap :: Map Object Coord
    , persistentState :: Set GroundedPredicate
    }

data Adj = Adj (Object, Object) deriving Eq
type Structure = ([Adj], [Adj])

type Location = Object
type Crate = Object


hAdjName = "hAdj"
vAdjName = "vAdj"
sokobanAtName = "sokobanAt"
atName = "at"

hAdj :: Location -> Location -> GroundedPredicate
hAdj from to = ("hAdj", [from, to])

vAdj :: Location -> Location -> GroundedPredicate
vAdj from to = ("vAdj", [from, to])

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
  let fromPos = objMap pddlw ! from
      toPos = objMap pddlw ! to
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

-- isGoalPred :: GroundedPredicate -> Bool
-- isGoalPred (goalName, loc) = True
-- isGoalPred _               = False
--
-- isAtPred :: GroundedPredicate -> Bool
-- isAtPred (atName, loc) = True
-- isAtPred _             = False

-- instance Ord Adj where
--     Adj (a, b) <= Adj (c, d)
--         | min a b <  min c d = True
--         | min a b >  min c d = False
--         | otherwise          = max a b <= max b a
--
-- adjacencyLists :: [GroundedPredicate] -> Structure
-- adjacencyLists adjPreds = adjLists adjPreds ([], []) where
--     adjLists :: [GroundedPredicate] -> Structure -> Structure
--     adjLists ((name, [a, b]) : rest) (verts, horzs)
--         | name == vAdjName = adjLists rest (Adj (a, b) : verts, horzs)
--         | name == hAdjName = adjLists rest (verts, Adj (a, b) : horzs)
--         | otherwise = adjLists rest (verts, horzs)
--     adjLists (_ : rest) (verts, horzs) = adjLists rest (verts, horzs)
--     adjLists [] adjs = adjs

-- tupleSort :: Ord a => ([a], [a]) -> ([a], [a])
-- tupleSort (a, b) = (sort a, sort b)


parseLocation :: Location -> Coord
parseLocation ('b' : n) = Coord (x, y) where
    (xStr, rest) = span isDigit n
    x = read xStr
    y = read $ tail rest
parseLocation n = error $ "Location " ++ n ++ " could not be parsed."

writeLocation :: Coord -> Location
writeLocation (Coord (x, y)) = ('b' : show x) ++ ('x' : show y)

statePred :: GroundedPredicate -> StatePred
statePred ("goal", [a]) = Goal a
statePred ("at", [c, l]) = CrateLoc c l
statePred ("sokobanAt", [l]) = SokobanLoc l
-- statePred (name, [a, b])
--     | name == hAdjName || name == vAdjName = Structure (min a b) (max a b)
statePred p = error $ "Could not understand predicate " ++ show p

data StatePred = Structure Location Location
               | CrateLoc Crate Location
               | SokobanLoc Location
               | Goal Location

fromState :: State -> SokobanPDDL
fromState state =
    let (structPreds, rest) = partition isStructurePred $ Set.toList state
        coordMap = Map.fromList
                 $ Set.toList
                 $ Set.map (\n -> (n, parseLocation n))
                 $ Set.unions
                 $ map (\(_, args) -> Set.fromList args) structPreds

        dataList = map statePred rest
        -- If this is an empty list, we throw an error, as we wouldn't be able
        -- to go on without knowing the location of Sokoban:
        sokoLoco = head [ l | SokobanLoc l <- dataList ]
        crates   = [ l | CrateLoc _ l <- dataList ]
        goals    = [ g | Goal g <- dataList ]

        tileMap  = Map.union
                   (Map.fromList $ zip (map (coordMap !) crates) (repeat Box))
                   (Map.fromList $ zip (Map.elems coordMap) (repeat Clear))

        world = World { coordMap = tileMap
                      , sokoban  = coordMap ! sokoLoco
                      , goals    = map (coordMap !) goals
                      }

        pddl = SokobanPDDL { world = world
                           , objMap = coordMap
                           , persistentState = Set.fromList structPreds
                           }

     in pddl

toState :: SokobanPDDL -> State
toState pddl = undefined
