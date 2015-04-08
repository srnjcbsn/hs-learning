module Learning.PDDLLearning where

import           Data.Typeable
import           Learning.Induction
import           Logic.Formula
import           Planning.PDDL
import           Planning.PDDL.Logic


import           Data.Map                (Map, (!))
import qualified Data.Map                as Map
import           Data.Set                (Set, (\\))
import qualified Data.Set                as Set
import qualified Data.TupleSet           as TSet
import qualified Learning.SchemaLearning as Lrn


type DomainKnowledge = Map Name (PreKnowledge, EffectKnowledge)
newtype PDDLKnowledge = PDDLKnowledge (DomainKnowledge)
                        deriving (Show, Eq, Typeable)

class Knowledge knl info question | knl -> info question where
    analyze :: knl -> info -> knl
    canAnswer :: knl -> question -> Bool
