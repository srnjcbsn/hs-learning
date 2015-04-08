module Learning.PDDL.OptimisticStrategy  where

import           Learning2
import           Learning.Induction
import qualified Learning.PDDL as Lrn
import           Learning.PDDL.NonConditionalKnowledge
import           Learning.PDDL.Experiment
import           Logic.Formula
import           Planning
import           Planning.PDDL
import           Planning.PDDL.Logic
import           Environment

import           Data.Function           (on)
import           Data.List               (deleteBy)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Set                (Set, (\\))
import qualified Data.Set                as Set
import           Data.Typeable


newtype (Environment env) =>
        OptimisticStrategy env = OptimisticStrategy ()



instance Strategy
            (OptimisticStrategy env)
            env
            PDDLKnowledge
            (PDDLExperiment env)
            Lrn.PDDLInfo
    where
      design strat knl = undefined

makeOptimisticDomain :: PDDLKnowledge -> PDDLDomain
makeOptimisticDomain = undefined
