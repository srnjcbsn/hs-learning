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


newtype (Environment env, ExternalPlanner planner PDDLDomain PDDLProblem ActionSpec) =>
        OptimisticStrategy planner env =
          OptimisticStrategy (planner)



instance (Environment env
         , ExternalPlanner planner PDDLDomain PDDLProblem ActionSpec)
          => Strategy
            (OptimisticStrategy planner env)
            env
            PDDLProblem
            PDDLKnowledge
            (PDDLExperiment env)
            Lrn.PDDLInfo
    where
      design strat@(OptimisticStrategy planner) prob knl@(PDDLKnowledge (_,_,s)) =
        do let optDom = makeOptimisticDomain knl
           let prob' = prob { probState = s}
           plan <- makePlan planner optDom prob'
           let expr = do plan' <- plan
                         return (PDDLExperiment plan',strat)
           return expr


makeOptimisticDomain :: PDDLKnowledge -> PDDLDomain
makeOptimisticDomain = undefined
