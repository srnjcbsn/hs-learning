module Learning.PDDL.NonConditionalKnowledge where

import           Data.Typeable
import qualified Learning.PDDL                       as Lrn
import qualified Learning.PDDL.EffectKnowledge       as Eff
import qualified Learning.PDDL.PreconditionKnowledge as Pre
import           Learning2
import           Planning
import           Planning.PDDL
import           Learning.Induction

import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set


type DomainKnowledge = Map Name (Pre.PreKnowledge, Eff.EffectKnowledge)

newtype PDDLKnowledge =
  PDDLKnowledge (PDDLDomain, DomainKnowledge, State)
    deriving (Show, Eq, Typeable)

updateKnowledge :: PDDLKnowledge -> Transition -> PDDLKnowledge
updateKnowledge (PDDLKnowledge (d,k,_)) trans@(_,(aname,_),s') =
  PDDLKnowledge (d, Map.adjust (f (uppPre,uppEff)) aname k,s')
  where f (f1,f2) (o1,o2) = (f1 o1, f2 o2)
        uppEff = flip (Eff.updateEffectHyp d) trans
        uppPre = flip (Pre.update d) trans


instance Knowledge PDDLKnowledge Lrn.PDDLInfo PDDLProblem where
    analyze knl (ts) = foldl updateKnowledge knl ts
    canAnswer (PDDLKnowledge (_, _, s)) prob =
      isSolved prob s

actionKnowledge :: [Name]
                -> [PredicateSpec]
                -> [Name]
                -> (Pre.PreKnowledge, Eff.EffectKnowledge)
actionKnowledge consts allPs paras =
    let paras' = map Ref paras ++ map Const consts
        unkns = Set.unions $ map (allFluents paras') allPs
        hyp = Lrn.Hyp (Set.empty, Set.empty) (unkns, unkns)
    in (Lrn.PreKnowledge hyp Set.empty, Lrn.EffKnowledge hyp)

initialKnowledge :: PDDLDomain -> State -> PDDLKnowledge
initialKnowledge dom s = PDDLKnowledge (dom, kn, s) where
    mapper aSpec = ( asName aSpec
                   , actionKnowledge (dmConstants dom)
                                     (dmPredicates dom)
                                     (asParas aSpec)
                   )
    kn = Map.fromList $ fmap mapper (dmActionsSpecs dom)
