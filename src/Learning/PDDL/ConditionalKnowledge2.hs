module Learning.PDDL.ConditionalKnowledge2 where

import           Data.TupleSet
import           Data.Typeable
import           Environment
import qualified Learning                            as Lrn
import qualified Learning.Induction                  as Ind
import qualified Learning.PDDL                       as PDDL
import qualified Learning.PDDL.EffectKnowledge       as Eff
import qualified Learning.PDDL.PreconditionKnowledge as Pre
import           Logic.Formula                       hiding (Formula)
import qualified Logic.Formula                       as LF
import           Planning
import           Planning.PDDL

import           Control.Monad                       (replicateM, sequence)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe
import           Data.Set                            (Set, (\\))
import qualified Data.Set                            as Set
import           Data.Tree
import           Data.TupleSet                       (TupleSet)
import qualified Data.TupleSet                       as TSet

-- type DomainKnowledge = Map Name Pattern

data Binding = Bound CArg
             | Free
             deriving (Eq, Ord, Show)

type Knowledge = TupleSet (Predicate Binding)

data Theory = Theory
    { known   :: Knowledge
    , unknown :: Knowledge
    }

data Cond = Cond
    { precs     :: Theory
    , precCands :: Set Knowledge
    }

type ActionTheory = Map (Predicate Int) Cond

type Subst = Map Object Int

type CArg = Int

data Literal a = Pos a
               | Neg a
               deriving (Eq, Ord, Show)

type LitPred a = Literal (Predicate a)
type Cands a = Set (Set (LitPred a))

type CondPred = LitPred Binding

data Pattern = Pattern
    { ctEffArg   :: CondPred
    , ctPreconds :: Set CondPred
    } deriving (Ord, Eq, Show)

data ConditionalKnowledge = ConditionalKnowledge
    { ckPattern :: Pattern
    , ckCands   :: Cands Binding
    , ckKnown   :: Set CondPred
    } deriving (Ord, Eq, Show)

newtype ActionKnowledge =
    ActionKnowledge (Map (Literal Name) ConditionalKnowledge)

newtype DomainKnowledge = DomainKnowledge (Map Name ActionKnowledge)

type Connection = (Int, Int, Literal Name)

connections :: CondPred -> Set CondPred -> [(Connection, CondPred)]
connections = undefined

onSuccess :: ConditionalKnowledge -> Pattern -> ConditionalKnowledge
onSuccess = undefined

onFailure :: ConditionalKnowledge -> Pattern -> ConditionalKnowledge
onFailure = undefined

failed :: [PredicateSpec]
       -> [Object]
       -> ActionKnowledge
       -> Transition
       -> [Pattern]
failed = undefined

succeeded :: [PredicateSpec] -> [Object] -> Transition -> [Pattern]
succeeded = undefined

updateKnowledge :: PDDLDomain
                -> PDDLProblem
                -> DomainKnowledge
                -> Transition
                -> DomainKnowledge
updateKnowledge = undefined
