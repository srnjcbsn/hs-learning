module Learning.PDDL where

import           Logic.Formula
import qualified Planning      as P
import           Planning.PDDL
-- import qualified Planning
-- import qualified Data.TupleSet as TSet
import           Data.TupleSet (TupleSet)

import           Control.Monad
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set


type PDDLInfo = [P.Transition]

class (P.Domain dom p as, Eq dh) => DomainHypothesis dh dom p as | dh -> dom p as where
    update :: dh -> dom -> P.Transition -> dh
    adjustDomain :: dh -> dom -> dom
    fromDomain :: dom -> dh

class (P.Domain d p as, Show d) => LearningDomain d p as | d -> p as where
   learn :: d -> P.Transition -> d

-- type Transition = (State, Action, State)

data Binding a b = Bound a
                 | Free b

type FBind = Binding Int Argument

-- | (Positive, Negative)
type Knowledge a = TupleSet (Predicate a)

type Cands a = Set (Knowledge a)

data Hyp a = Hyp
    { knowns   :: Knowledge a
    , unknowns :: Knowledge a
    } deriving (Eq, Show)

posKnown, posUnknown, negKnown, negUnknown :: Hyp a -> Set (Predicate a)
posKnown   = fst . knowns
posUnknown = fst . unknowns
negKnown   = snd . knowns
negUnknown = snd . unknowns

data PreKnowledge a = PreKnowledge (Hyp a) (Cands a) deriving (Eq, Show)
data EffKnowledge a = EffKnowledge (Hyp a) deriving (Eq, Show)

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

tmp :: [PredicateSpec] -> [Object] -> ActionHyp -> P.Transition -> ActionHyp
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
