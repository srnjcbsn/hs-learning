module Learning.PDDL.OptimisticStrategy  where

import qualified Learning.PDDL.EffectKnowledge       as Eff
import qualified Learning.PDDL.PreconditionKnowledge as Pre
import           Learning
import qualified Learning.PDDL as Lrn
import           Learning.PDDL.NonConditionalKnowledge
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
             (PDDLKnowledge env)
             (PDDLExperiment env)
             (Lrn.PDDLInfo env)
    where
      design strat@(OptimisticStrategy (planner, bound)) prob knl@(PDDLKnowledge (_,_,s)) =
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


effectSchema :: Eff.EffectKnowledge
                      -> ActionSpec
                      -> ActionSpec
effectSchema (Lrn.EffKnowledge kn) aSpec = aSpec { asEffect = eff } where
    addL = Set.map Pred $ (Lrn.posUnknown kn) `Set.union` (Lrn.posKnown kn)
    delL = Set.map (Neg . Pred) (Lrn.negKnown kn)
    eff = Con $ Set.toList $ addL `Set.union` delL

precondFormula :: Pre.PreKnowledge -> Formula Argument
precondFormula (Lrn.PreKnowledge hyp cnf) =
    Con predList
    where negPredList (poss,negs) =  Set.toList (Set.map Pred negs)
                                  ++ Set.toList (Set.map (Neg . Pred) poss)
          orList = Neg . Con . negPredList
          predList =  Set.toList (Set.map Pred (Lrn.posKnown hyp))
                   ++ Set.toList (Set.map (Neg . Pred) (Lrn.negKnown hyp))
                   ++ Set.toList (Set.map orList cnf)

precondSchema :: Pre.PreKnowledge -> ActionSpec -> ActionSpec
precondSchema preKnow aSpec = aSpec { asPrecond = precondFormula preKnow }

mkSchema :: DomainKnowledge -> ActionSpec -> ActionSpec
mkSchema kn as = case Map.lookup (asName as) kn of
    Just (pk, ek) -> (effectSchema ek . precondSchema pk) as
    Nothing -> error $  "Optimistic schema construction: lookup of action schema"
                     ++ (asName as) ++ " failed."

makeOptimisticDomain :: PDDLKnowledge env -> PDDLDomain
makeOptimisticDomain (PDDLKnowledge (dom, kn, _)) = dom { dmActionsSpecs = as }
    where as = map (mkSchema kn) (dmActionsSpecs dom)
