module Learning.PDDL.NonConditionalKnowledge where

import           Data.Typeable
import           Environment
import           Learning
import           Learning.Induction
import qualified Learning.PDDL                       as Lrn
import qualified Learning.PDDL.EffectKnowledge       as Eff
import qualified Learning.PDDL.PreconditionKnowledge as Pre
import           Planning
import           Planning.PDDL
import Data.UnsafeMap

import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Set                            (Set)
import qualified Data.Set                            as Set


type DomainKnowledge = Map Name (Pre.PreKnowledge, Eff.EffectKnowledge)

newtype Environment env => PDDLKnowledge env =
  PDDLKnowledge (PDDLDomain, DomainKnowledge, State)
    deriving (Show, Eq, Typeable)

domainKnowledge :: PDDLKnowledge env -> DomainKnowledge
domainKnowledge (PDDLKnowledge (_, dk, _)) = dk

pddlDomain :: PDDLKnowledge env -> PDDLDomain
pddlDomain (PDDLKnowledge (dom, _, _)) = dom

knlFromDomKnl :: DomainKnowledge
              -> Name
              -> (Pre.PreKnowledge, Eff.EffectKnowledge)
knlFromDomKnl dmknl actname =
  unsLookup
    ("no action " ++ actname ++ " in " ++ dmknl)
    actname
    dmknl


updateKnowledge :: PDDLKnowledge env -> Transition -> PDDLKnowledge env
updateKnowledge (PDDLKnowledge (dom, dk, _)) trans@(_, (aname, _), s') =
  PDDLKnowledge (dom, dk', s')
  where dk' = Map.adjust (f (uppPre, uppEff)) aname dk
        f (f1, f2) (o1, o2) = (f1 o1, f2 o2)
        uppEff = flip (Eff.updateEffectHyp dom) trans
        uppPre = flip (Pre.update dom) trans

instance Environment env => Knowledge (PDDLKnowledge env) (Lrn.PDDLInfo env) PDDLProblem where
    analyze knl info = foldl updateKnowledge knl (Lrn.transitions info)
    canAnswer (PDDLKnowledge (_, _, s)) prob = isSolved prob s

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

actionKnowledgeEff :: [Name]
                -> [PredicateSpec]
                -> [Name]
                -> Eff.EffectKnowledge
actionKnowledgeEff consts allPs paras =
    let unkns = allPreds consts allPs paras
        hyp = Lrn.Hyp (Set.empty, Set.empty) (unkns, unkns)
    in Lrn.EffKnowledge hyp

actionKnowledge :: [Name]
                -> [PredicateSpec]
                -> [Name]
                -> (Pre.PreKnowledge, Eff.EffectKnowledge)
actionKnowledge consts allPs paras =
    let unkns = allPreds consts allPs paras
        hyp = Lrn.Hyp (Set.empty, Set.empty) (unkns, unkns)
    in (Lrn.PreKnowledge hyp Set.empty, Lrn.EffKnowledge hyp)

initialKnowledge :: PDDLDomain -> State -> PDDLKnowledge env
initialKnowledge dom s = PDDLKnowledge (dom, kn, s) where
    mapper aSpec = ( asName aSpec
                   , actionKnowledge (dmConstants dom)
                                     (dmPredicates dom)
                                     (asParas aSpec)
                   )
    kn = Map.fromList $ fmap mapper (dmActionsSpecs dom)
