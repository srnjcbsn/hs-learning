module Learning.PDDLLearning where

import           Data.Typeable
import           Environment
import           Learning.Induction
import qualified Learning.PDDL                       as Lrn
import qualified Learning.PDDL.EffectKnowledge       as Eff
import qualified Learning.PDDL.PreconditionKnowledge as Pre
import           Logic.Formula
import           Planning.PDDL
import           Planning.PDDL.Logic

import           Data.Map                            (Map, (!))
import qualified Data.Map                            as Map
import           Data.Set                            (Set, (\\))
import qualified Data.Set                            as Set
import qualified Data.TupleSet                       as TSet

data Environment env => PDDLSimStep env = PDDLSimStep
    { step   :: Int
    , knl    :: (Eff.EffectKnowledge, Pre.PreKnowledge)
    , action :: Action
    , env    :: env
    }
