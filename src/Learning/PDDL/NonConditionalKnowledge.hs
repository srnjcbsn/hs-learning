module Learning.PDDL.NonConditionalKnowledge where

import           Data.Typeable
import           Learning.Induction
import           Logic.Formula
import           Planning.PDDL
import           Planning.PDDL.Logic
import           Learning.PDDL.EffectKnowledge
import           Learning.PDDL.PreconditionKnowledge

import           Data.Map                (Map, (!))
import qualified Data.Map                as Map
import           Data.Set                (Set, (\\))
import qualified Data.Set                as Set
import qualified Data.TupleSet           as TSet
import qualified Learning.SchemaLearning as Lrn
import           Learning2

type DomainKnowledge = Map Name (PreKnowledge, EffectKnowledge)
newtype PDDLKnowledge = PDDLKnowledge (DomainKnowledge)
                        deriving (Show, Eq, Typeable)

newtype PDDLInfo = PDDLInfo [Lrn.Transition]
newtype PDDLQuestion  = PDDLQuestion (Formula Name)

instance Knowledge PDDLKnowledge PDDLInfo PDDLQuestion where
    analyze knl info = undefined
    canAnswer knl quest = undefined
