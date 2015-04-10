module Learning.PDDL.ConditionalKnowledge where

import           Data.TupleSet
import           Data.Typeable
import           Environment
import qualified Learning                            as L
import           Learning.Induction
import qualified Learning.PDDL                       as Lrn
import qualified Learning.PDDL.EffectKnowledge       as Eff
import qualified Learning.PDDL.PreconditionKnowledge as Pre
import           Logic.Formula
import           Planning
import           Planning.PDDL

import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Set                            (Set, (\\))
import qualified Data.Set                            as Set


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

h :: [Object] -> Subst -> Int -> Subst
h [] _ _ = Map.empty
h (o : os) m n =
    case Map.lookup o m of
        Just i  -> h os m n
        Nothing -> h os (Map.insert o n m) (n + 1)

ug :: GroundedPredicate -> Subst
ug (Predicate n objs) = undefined

initActionTheory :: Predicate Int -> State -> Subst -> ActionTheory
initActionTheory = undefined

f :: ActionTheory -> Transition -> ActionTheory
f at (s, a, s') =
    let posEffs = s' \\ s
        posEffsUg =undefined

    in undefined
