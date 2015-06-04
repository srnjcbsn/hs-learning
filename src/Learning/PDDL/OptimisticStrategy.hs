module Learning.PDDL.OptimisticStrategy  where

import qualified Learning.PDDL.NonConditionalTypes as NCT

import           Learning
import qualified Learning.PDDL as Lrn
-- import           Learning.PDDL.NonConditionalKnowledge
import           Learning.PDDL.Experiment
import           Logic.Formula
import           Planning
import           Planning.PDDL
import           Environment

import qualified Data.Map                as Map
import qualified Data.Set                as Set

newtype ( Environment env
        , BoundedPlanner planner
        , ExternalPlanner planner PDDLDomain PDDLProblem ActionSpec) =>
        OptimisticStrategy planner env = OptimisticStrategy (planner, Maybe Int)

instance ( Environment env
         , BoundedPlanner planner
         , ExternalPlanner planner PDDLDomain PDDLProblem ActionSpec
         ) => Strategy
             (OptimisticStrategy planner env)
             env
             PDDLProblem
             (NCT.PDDLKnowledge env)
             (PDDLExperiment env)
             (Lrn.PDDLInfo env)
    where
      design strat@(OptimisticStrategy (planner, bound)) prob knl@(NCT.PDDLKnowledge (_,_,s)) =
        do let optDom = makeOptimisticDomain knl
           let prob' = prob { probState = s}
           let planner' = setBound planner bound
           plan <- makePlan planner' optDom prob'
           let expr = do plan' <- plan
                         return (PDDLExperiment plan' optDom, strat)
           return expr

      update (OptimisticStrategy (planner, bound)) (PDDLExperiment p _) (Lrn.PDDLInfo _ _ n)
        | n == length p =
          case bound of
              Just b -> (OptimisticStrategy (planner, Just (b * 2)))
              Nothing -> (OptimisticStrategy (planner, Nothing))
        | otherwise = OptimisticStrategy (planner, Just 1)


effectSchema :: NCT.EffKnowledge
                      -> ActionSpec
                      -> ActionSpec
effectSchema (NCT.EffKnowledge kn) aSpec = aSpec { asEffect = eff } where
    addL = Set.map Pred $ (NCT.posUnknown kn) `Set.union` (NCT.posKnown kn)
    delL = Set.map (Neg . Pred) (NCT.negKnown kn)
    eff = Con $ Set.toList $ addL `Set.union` delL

precondFormula :: NCT.PreKnowledge -> Formula Argument
precondFormula (NCT.PreKnowledge hyp cnf) =
    Con predList
    where negPredList (poss,negs) =  Set.toList (Set.map Pred negs)
                                  ++ Set.toList (Set.map (Neg . Pred) poss)
          orList = Neg . Con . negPredList
          predList =  Set.toList (Set.map Pred (NCT.posKnown hyp))
                   ++ Set.toList (Set.map (Neg . Pred) (NCT.negKnown hyp))
                   ++ Set.toList (Set.map orList cnf)

precondSchema :: NCT.PreKnowledge -> ActionSpec -> ActionSpec
precondSchema preKnow aSpec = aSpec { asPrecond = precondFormula preKnow }

mkSchema :: NCT.DomainKnowledge -> ActionSpec -> ActionSpec
mkSchema kn as = case Map.lookup (asName as) kn of
    Just (pk, ek) -> (effectSchema ek . precondSchema pk) as
    Nothing -> error $  "Optimistic schema construction: lookup of action schema"
                     ++ (asName as) ++ " failed."

makeOptimisticDomain :: NCT.PDDLKnowledge env -> PDDLDomain
makeOptimisticDomain (NCT.PDDLKnowledge (dom, kn, _)) = dom { dmActionsSpecs = as }
    where as = map (mkSchema kn) (dmActionsSpecs dom)
