module Learning.SchemaLearning where

import           Logic.Formula
import qualified Planning      as P
import           Planning.PDDL
-- import qualified Data.TupleSet as TSet
import           Data.TupleSet (TupleSet)

import           Data.Map      (Map)
import qualified Data.Map      as Map
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

data ForAllHyp = ForAllHyp
    { parameters :: [P.Type]
    , cEffects   :: [ConditionalEffect]
    }

data ActionHyp = ActionHyp
    { name       :: String
    , params     :: [(Name, P.Type)]
    , consts     :: [(Name, P.Type)]
    , preconds   :: PreKnowledge Argument
    , certainEff :: EffKnowledge Argument
    , condEff    :: ForAllHyp
    }

type DomainHyp = Map Name ActionHyp

typeOccurrences :: P.Type -> PredicateSpec -> Int
typeOccurrences t = foldr (typeSum . snd) 0 . predArgs
    where typeSum t' | t' == t   = (+) 1
                     | otherwise = id

maxTypeOccurrence :: P.Type -> [PredicateSpec] -> Int
maxTypeOccurrence t specs = maximum $ map (typeOccurrences t) specs

forAllFromDomain :: [P.Type] -> [PredicateSpec] -> ForAllHyp
-- TODO: The content of the forall should no tbe the empty list, but a single
--       conditional with no preconds and one of each effect (bound to the
--       variables defined by fTypes)
forAllFromDomain types preds = ForAllHyp fTypes [] where
    fTypes = concatMap (\t -> replicate (maxTypeOccurrence t preds) t) types

fromActionSpec :: [P.Type] -> [PredicateSpec] -> ActionSpec -> ActionHyp
fromActionSpec types precs aSpec = ActionHyp
    { name = asName aSpec
    , params = Map.toList $ asTypes aSpec
    -- FIXME: Constants in actionspec should have embedded type information
    , consts = zip (asConstants aSpec) (repeat baseType)
    , preconds = undefined
    , certainEff = undefined
    , condEff = forAllFromDomain types precs
    }

initialHypothesis :: PDDLDomain -> ActionSpec -> DomainHyp
initialHypothesis = undefined
