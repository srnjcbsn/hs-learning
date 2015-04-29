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
type Knowledge a = TupleSet (Predicate a)

type Cands a = Set (Knowledge a)

data Hyp a = Hyp
    { knowns   :: Knowledge a
    , unknowns :: Knowledge a
    } deriving (Eq, Ord, Show)

posKnown, posUnknown, negKnown, negUnknown :: Hyp a -> Set (Predicate a)
posKnown   = fst . knowns
posUnknown = fst . unknowns
negKnown   = snd . knowns
negUnknown = snd . unknowns

data PreKnowledge a = PreKnowledge (Hyp a) (Cands a) deriving (Eq, Ord, Show)
data EffKnowledge a = EffKnowledge (Hyp a) deriving (Eq, Ord, Show)

deltaHyp :: Ord a => Hyp a -> Hyp a -> Hyp a
deltaHyp h1 h2 = Hyp ks us where
    ks = (knowns h2) `TSet.difference` (knowns h1)
    us = (unknowns h1) `TSet.difference` (unknowns h2)

pkHyp :: PreKnowledge a -> Hyp a
pkHyp (PreKnowledge h _) = h

ekHyp :: EffKnowledge a -> Hyp a
ekHyp (EffKnowledge h) = h

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
