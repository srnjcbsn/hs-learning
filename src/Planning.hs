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

aArgs :: Action -> [Object]
aArgs = snd

aName :: Action -> Name
aName = fst

class (Domain d as, Problem p) => ExternalPlanner ep d p as where
    makePlan :: ep -> d -> p -> IO (Maybe Plan)

class ActionSpecification as => Domain d as | d -> as where
    actionSpecification :: d -> Name -> as
    actions             :: d -> [as]
    apply               :: d -> State -> Action -> State

class Problem p where
    initialState :: p -> State
    isSolved     :: p -> State -> Bool
    objects      :: p -> [Object]

class ActionSpecification a where
    name           :: a -> String
    arity          :: a -> Int
    isApplicable   :: a -> State -> [Name] -> Bool
    effect         :: a -> State -> [Name] -> TupleSet GroundedPredicate
