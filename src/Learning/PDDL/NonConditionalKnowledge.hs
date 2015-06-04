module Learning.PDDL.NonConditionalKnowledge where

import qualified Learning.PDDL.EffectKnowledge as Eff
import qualified Learning.PDDL.PreconditionKnowledge as Pre

import Learning.PDDL.NonConditionalTypes
-- import Data.TupleSet (TupleSet)
-- import qualified Data.TupleSet as TSet
-- import Logic.Formula
import           Environment
import qualified Learning as Lrn
import qualified Learning.PDDL as PDDL
import           Learning.Induction
import           Planning
import           Planning.PDDL

-- import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Set                            (Set)
import qualified Data.Set                            as Set

instance Environment env => Lrn.Knowledge (PDDLKnowledge env) (PDDL.PDDLInfo env) PDDLProblem where
    analyze knl info = foldl updateKnowledge knl (PDDL.transitions info)
    canAnswer (PDDLKnowledge (_, _, s)) prob = isSolved prob s

domainKnowledge :: PDDLKnowledge env -> DomainKnowledge
domainKnowledge (PDDLKnowledge (_, dk, _)) = dk

pddlDomain :: PDDLKnowledge env -> PDDLDomain
pddlDomain (PDDLKnowledge (dom, _, _)) = dom

updateKnowledge :: PDDLKnowledge env -> Transition -> PDDLKnowledge env
updateKnowledge (PDDLKnowledge (dom, dk, _)) trans@(_, (aname, _), s') =
  PDDLKnowledge (dom, dk', s')
  where dk' = Map.adjust (f (uppPre, uppEff)) aname dk
        f (f1, f2) (o1, o2) = (f1 o1, f2 o2)
        uppEff = flip (Eff.updateEffectKnl dom) trans
        uppPre = flip (Pre.update dom) trans

allPreds :: [Name]
         -> [PredicateSpec]
         -> [Name]
         -> Set FluentPredicate
allPreds consts pSpecs paras = fPreds where
    allParas = map Ref paras ++ map Const consts
    fPreds = Set.unions $ map (allFluents allParas) pSpecs

allPredsForAction :: PDDLDomain -> Name -> Set FluentPredicate
allPredsForAction dom n =
    let aSpec = unsActionSpec dom n
    in allPreds (dmConstants dom) (dmPredicates dom) (asParas aSpec)

actionKnowledge :: [Name]
                -> [PredicateSpec]
                -> [Name]
                -> (PreKnowledge, EffKnowledge)
actionKnowledge consts allPs paras =
    let unkns = allPreds consts allPs paras
        knl = Knowledge (Set.empty, Set.empty) (unkns, unkns)
    in (PreKnowledge knl Set.empty, EffKnowledge knl)

initialKnowledge :: PDDLDomain -> State -> PDDLKnowledge env
initialKnowledge dom s = PDDLKnowledge (dom, kn, s) where
    mapper aSpec = ( asName aSpec
                   , actionKnowledge (dmConstants dom)
                                     (dmPredicates dom)
                                     (asParas aSpec)
                   )
    kn = Map.fromList $ fmap mapper (dmActionsSpecs dom)

