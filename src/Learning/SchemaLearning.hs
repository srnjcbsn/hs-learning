module Learning.SchemaLearning where

import           Logic.Formula
import qualified Planning      as P
import           Planning.PDDL
-- import qualified Data.TupleSet as TSet
import           Data.TupleSet (TupleSet)

import           Control.Monad
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set

type Transition = (State, Action, State)

data Binding a b = Bound a
                 | Free b

type FBind = Binding Int Argument

type Cands a = Set (Knowledge a)

-- | (Positive, Negative)
type Knowledge a = TupleSet (Predicate a)

data Hyp a = Hyp
    { knowns   :: Knowledge a
    , unknowns :: Knowledge a
    }

data PreKnowledge a = PreKnowledge (Hyp a) (Cands a)
data EffKnowledge a = EffKnowledge (Hyp a)

type ConditionalEffect = (PreKnowledge FBind, EffKnowledge FBind)

data ForAllHyp = ForAllHyp
    { parameters :: [P.Type]
    , cEffects   :: [ConditionalEffect]
    }

data ActionHyp = ActionHyp
    { name        :: String
    , params      :: [(Name, P.Type)]
    , consts      :: [(Name, P.Type)]
    , condEffs    :: [ForAllHyp]
    , commonConds :: PreKnowledge FBind
    }

type DomainHyp = Map Name ActionHyp

typeOccurrences :: P.Type -> PredicateSpec -> Int
typeOccurrences t = foldr (typeSum . snd) 0 . predArgs
    where typeSum t' | t' == t   = (+) 1
                     | otherwise = id

maxTypeOccurrence :: P.Type -> [PredicateSpec] -> Int
maxTypeOccurrence t specs = maximum $ map (typeOccurrences t) specs

allPreds :: PredicateSpec -> [Object] -> Set GroundedPredicate
allPreds ps objs = Set.fromList
                 $ map (Predicate (predName ps))
                 $ replicateM (length . predArgs $ ps) objs

-- fBindPred :: PredicateSpec -> Predicate FBind
-- fBindPred ps = Predicate (predName ps) (take (predArity ps) [1 .. ])

posOrNegCand :: PredicateSpec
             -> [Object]
             -> State
             -> TupleSet PredicateSpec
             -> TupleSet PredicateSpec
posOrNegCand ps objs s (pos, neg)
    | Set.isSubsetOf (allPreds ps objs) s =
        (pos, Set.insert ps neg)
    | Set.null (Set.intersection (allPreds ps objs) s) =
        (Set.insert ps pos, neg)
    | otherwise = (pos, neg)

tmp :: [PredicateSpec] -> [Object] -> ActionHyp -> Transition -> ActionHyp
tmp ps objs ah t@(s, a, s') | s == s' = undefined

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
    , condEffs = [forAllFromDomain types precs]
    }

initialHypothesis :: PDDLDomain -> ActionSpec -> DomainHyp
initialHypothesis = undefined
