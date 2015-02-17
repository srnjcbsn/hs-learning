module Environments.Sokoban.PDDL where

import Environments.Sokoban.Sokoban hiding (Object)
import PDDL.Type

import qualified Data.Map as Map
import Data.Map (Map,(!))

data SokobanPDDL = SokobanPDDL
    { world  :: World
    , objMap :: Map Object Coord
    }

type Location = Object
type Crate = Object


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
   in case (toPos - fromPos) of
        Coord (0, 1) -> UpDir
        Coord (0, -1) -> DownDir
        Coord (1, 0) -> RightDir
        Coord (-1, 0) -> LeftDir
        _ -> error ("cannot get direction (fromPos: " ++ show fromPos ++", toPos: " ++ show toPos ++ ")")


applyAction :: SokobanPDDL -> Action -> SokobanPDDL
applyAction pddlw ("move-h", [from, to]) = pddlw { world = move (world pddlw) $ directionFromObjs pddlw from to}
applyAction pddlw ("move-v", args) = undefined
applyAction pddlw ("push-h", args) = undefined
applyAction pddlw ("push-v", args) = undefined
applyAction pddlw ("push-h-goal", args) = undefined
applyAction pddlw ("push-v-goal", args) = undefined
applyAction _ act = error ("Unknown action: " ++ show act)

fromState :: State -> SokobanPDDL
fromState = undefined

toState :: SokobanPDDL -> State
toState = undefined
