module Learning.PDDL.ConditionalKnowledge where

import           Data.TupleSet
import           Data.Typeable
import           Environment
import qualified Learning                            as Lrn
import           Learning.Induction
import qualified Learning.PDDL                       as PDDL
import qualified Learning.PDDL.EffectKnowledge       as Eff
import qualified Learning.PDDL.PreconditionKnowledge as Pre
import           Logic.Formula
import           Planning
import           Planning.PDDL

import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Set                            (Set, (\\))
import qualified Data.Set                            as Set
import           Data.Tree
import           Data.TupleSet


data Binding = Bound Int
             | Free  Int

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

type CondKnl = PDDL.Knowledge CArg

data KNode = KNode
    { cands :: PDDL.Cands CArg
    , unkns :: CondKnl
    }

data CondTheory = CondTheory
    { ctEffects :: CondKnl
    , ctRevTree :: RevTree
    }

type RevTree = Tree KNode

type Pattern = (CondKnl, CondKnl)

-- | Merge two pieces of precondition knowledge, producing a set of revisions
merge :: CondKnl -> CondKnl -> [CondKnl]
merge = undefined

-- | Updates the given theory with the knowledge obtained in the new transition
updateTheory :: CondTheory -> CondKnl -> CondTheory
updateTheory = undefined

mergeEffects :: CondTheory -> Pattern -> CondTheory
mergeEffects = undefined

update :: CondTheory -> Transition -> CondTheory
update = undefined
