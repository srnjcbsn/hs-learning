module Learning.PDDL.Experiment where

import Environment
import Learning2
import           Learning.Induction
import qualified Learning.PDDL as Lrn
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


newtype (Environment env) => PDDLExperiment env = PDDLExperiment [Action]

instance Experiment (PDDLExperiment env) env Lrn.PDDLInfo where
    conduct = undefined
