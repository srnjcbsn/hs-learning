module Learning.PDDL.NonConditionalTypes where

import           Data.TupleSet (TupleSet)
import qualified Data.TupleSet as TSet
import           Environment
import           Logic.Formula
import           Planning
import           Planning.PDDL

import           Data.Map      (Map)
import           Data.Set      (Set)

type Cands = Set (LiteralSet Term)
data PreKnowledge = PreKnowledge Knowledge Cands deriving (Eq, Ord, Show)
data EffKnowledge = EffKnowledge Knowledge deriving (Eq, Ord, Show)

type DomainKnowledge = Map Name (PreKnowledge, EffKnowledge)

newtype Environment env => PDDLKnowledge env =
  PDDLKnowledge (PDDLDomain, DomainKnowledge, State, AllPossibleObjects)
    deriving (Show, Eq)

-- | (Positive, Negative)
type LiteralSet a = TupleSet (Predicate a)

data Knowledge = Knowledge
    { knowns   :: LiteralSet Term
    , unknowns :: LiteralSet Term
    } deriving (Eq, Ord, Show)

posKnown, posUnknown, negKnown, negUnknown :: Knowledge -> Set (Predicate Term)
posKnown   = fst . knowns
posUnknown = fst . unknowns
negKnown   = snd . knowns
negUnknown = snd . unknowns

deltaKnl :: Knowledge -> Knowledge -> Knowledge
deltaKnl h1 h2 = Knowledge ks us where
    ks = knowns h2 `TSet.difference` knowns h1
    us = unknowns h1 `TSet.difference` unknowns h2

knlFromPk :: PreKnowledge -> Knowledge
knlFromPk (PreKnowledge h _) = h

knlFromEk :: EffKnowledge -> Knowledge
knlFromEk (EffKnowledge h) = h
