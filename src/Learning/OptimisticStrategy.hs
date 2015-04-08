module Learning.OptEffectLearn  where

import           Learning.Induction
import qualified Learning.SchemaLearning as Lrn
import           Logic.Formula
import           Planning
import           Planning.PDDL
import           Planning.PDDL.Logic

import           Data.Function           (on)
import           Data.List               (deleteBy)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Set                (Set, (\\))
import qualified Data.Set                as Set
import           Data.Typeable


class Experiment exp world info | exp -> world info  where
    conduct :: exp -> world -> IO (info, world)

class Experiment exp world info => Strategy strat world knl exp info | strat -> exp knl where
    design :: strat -> knl -> Maybe exp
