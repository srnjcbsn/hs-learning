module Learning.SchemaLearning where

import           Logic.Formula
import           Planning
import           Planning.PDDL
-- import qualified Data.TupleSet as TSet
import           Data.TupleSet (TupleSet)

import           Data.Set      (Set)

data Binding a b = Bound a
                 | Free b

type FBind = Binding Int Argument

type CNF a = Set (Knowledge a)

-- | (Positive, Negative)
type Knowledge a = TupleSet (Predicate a)

data Hyp a = Hyp
    { knowns   :: Knowledge a
    , unknowns :: Knowledge a
    }

data PreKnowledge a = PreKnowledge (Hyp a) (CNF a)
data EffKnowledge a = EffKnowledge (Hyp a)

type ConditionalEffect = (PreKnowledge FBind, EffKnowledge FBind)

data ForallHyp = ForallHyp
    { parameters :: [Type]
    , cEffects   :: [ConditionalEffect]
    }

data ActionHyp = ActionHyp
    { name       :: String
    , params     :: [(Name, Type)]
    , consts     :: [(Name, Type)]
    , preconds   :: PreKnowledge Argument
    , certainEff :: EffKnowledge Argument
    , condEff    :: ForallHyp
    }
