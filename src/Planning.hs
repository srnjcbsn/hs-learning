module Planning where

import           Data.Set      (Set)

import           Data.TupleSet (TupleSet)

type Object = String
type Name = String


type GroundedPredicate = (Name, [Object])
type State = Set GroundedPredicate

type Action = (Name, [Object])
type Plan = [Action]

aArgs :: Action -> [Object]
aArgs = snd

aName :: Action -> Name
aName = fst

class (Domain d as, Problem p) => ExternalPlanner ep d p as | p -> d as where
    makePlan :: ep -> d -> p -> IO (Maybe Plan)

class ActionSpecification as => Domain d as | d -> as where
    actionSpecification :: d -> Name -> Maybe as
    actions             :: d -> [as]
    apply               :: d -> State -> Action -> Maybe State

class Problem p where
    initialState :: p -> State
    isSolved     :: p -> State -> Bool
    objects      :: p -> [Object]

class ActionSpecification a where
    name           :: a -> String
    arity          :: a -> Int
    isApplicable   :: a -> State -> [Name] -> Bool
    effect         :: a -> [Name] -> TupleSet GroundedPredicate