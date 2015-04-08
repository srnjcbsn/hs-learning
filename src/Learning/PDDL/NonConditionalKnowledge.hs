module Learning.PDDL.NonConditionalKnowledge where

import           Data.Typeable
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
import qualified Learning.PDDL                       as Lrn
import           Learning2
import           Planning

type DomainKnowledge = Map Name (Pre.PreKnowledge, Eff.EffectKnowledge)

newtype PDDLKnowledge =
  PDDLKnowledge (PDDLDomain, DomainKnowledge, State)
    deriving (Show, Eq, Typeable)

updateKnowledge :: PDDLKnowledge -> Transition -> PDDLKnowledge
updateKnowledge (PDDLKnowledge (d,k,_)) trans@(_,(aname,_),s') =
  PDDLKnowledge (d, Map.adjust (f (uppPre,uppEff)) aname k,s')
  where f (f1,f2) (o1,o2) = (f1 o1, f2 o2)
        uppEff = flip (Eff.updateEffectHyp d) $ trans
        uppPre = flip (Pre.update d) $ trans


instance Knowledge PDDLKnowledge Lrn.PDDLInfo Lrn.PDDLQuestion where
    analyze knl (Lrn.PDDLInfo ts) = foldl updateKnowledge knl ts
    canAnswer (PDDLKnowledge (_,_,s)) (Lrn.PDDLQuestion (goal)) =
      isSatisfied goal s
