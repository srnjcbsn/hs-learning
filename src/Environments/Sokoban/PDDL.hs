module Environments.Sokoban.PDDL where

import Environments.Sokoban.Sokoban hiding (Object)
import PDDL.Type

import qualified Data.Map as Map
import Data.Map (Map)

data SokobanPDDL = SokobanPDDL
    { world  :: World
    , objMap :: Map Object Coord
    }

type Location = Object
type Crate = Object

movH = "move-h"
movV = "move-v"
pushH = "push-h"
pushV = "push-v"
pushHGoal = "push-h-goal"
pushVGoal = "push-v-goal"

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

applyAction :: SokobanPDDL -> GroundedAction -> SokobanPDDL
applyAction = undefined

fromState :: State -> SokobanPDDL
fromState = undefined

toState :: SokobanPDDL -> State
toState = undefined
