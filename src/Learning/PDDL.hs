module Learning.PDDL where

import           Logic.Formula
import qualified Planning      as P
import           Planning.PDDL
import Environment
import           Data.TupleSet (TupleSet)
import qualified Data.TupleSet as TSet

import           Control.Monad
import           Data.Map      (Map)
import           Data.Set      (Set)
import qualified Data.Set      as Set

data Environment env => PDDLInfo env = PDDLInfo
    { transitions :: [P.Transition]
    , states      :: [env]
    , stepsDone   :: Int
    } deriving Show

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
type LiteralSet a = TupleSet (Predicate a)

type Cands a = Set (LiteralSet a)

data Knowledge a = Knowledge
    { knowns   :: LiteralSet a
    , unknowns :: LiteralSet a
    } deriving (Eq, Ord, Show)

posKnown, posUnknown, negKnown, negUnknown :: Knowledge a -> Set (Predicate a)
posKnown   = fst . knowns
posUnknown = fst . unknowns
negKnown   = snd . knowns
negUnknown = snd . unknowns

data PreKnowledge a = PreKnowledge (Knowledge a) (Cands a) deriving (Eq, Ord, Show)
data EffKnowledge a = EffKnowledge (Knowledge a) deriving (Eq, Ord, Show)

deltaKnl :: Ord a => Knowledge a -> Knowledge a -> Knowledge a
deltaKnl h1 h2 = Knowledge ks us where
    ks = knowns h2 `TSet.difference` knowns h1
    us = unknowns h1 `TSet.difference` unknowns h2

knlFromPk :: PreKnowledge a -> Knowledge a
knlFromPk (PreKnowledge h _) = h

knlFromEk :: EffKnowledge a -> Knowledge a
knlFromEk (EffKnowledge h) = h

type ConditionalEffect = (PreKnowledge FBind, EffKnowledge FBind)

data ForAllKnowledge = ForAllKnowledge
    { parameters :: [P.Type]
    , cEffects   :: [ConditionalEffect]
    }

data ActionHyp = ActionHyp
    { name        :: String
    , params      :: [(Name, P.Type)]
    , consts      :: [(Name, P.Type)]
    , condEffs    :: [ForAllKnowledge]
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
