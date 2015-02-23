{-# LANGUAGE MultiParamTypeClasses #-}

module Planning where

import           Data.TupleSet (TupleSet)
import qualified Data.TupleSet as TSet

import           Data.Set      (Set)
import qualified Data.Set      as Set

type Object = String
type Name = String

type GroundedPredicate = (Name, [Object])
type State = Set GroundedPredicate

type Action = (Name, [Object])
type Plan = [Action]

class (Domain d, Problem p) => ExternalPlanner ep d p where
    makePlan :: ep -> d -> p -> IO (Maybe Plan)

class Domain d where
    actionSpecification :: ActionSpecification a => d -> Name -> a
    actions             :: ActionSpecification a => d -> [a]
    apply               :: ActionSpecification a => d -> a -> State -> [Name] -> State

class Problem p where
    initialState :: p -> State
    isSolved     :: p -> State -> Bool

class ActionSpecification a where
    name           :: a -> String
    arity          :: a -> Int
    isApplicable   :: a -> State -> [Name] -> Bool
    effect         :: a -> State -> [Name] -> TupleSet GroundedPredicate
