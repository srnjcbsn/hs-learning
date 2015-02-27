module Planning where

import           Data.Set      (Set)
import Control.Monad (replicateM)
import           Data.TupleSet (TupleSet)

import Logic.Formula

type Object = String
type Name   = String
type Type   = String

type GroundedPredicate = Predicate Object
type State = Set GroundedPredicate

type Action = (Name, [Object])
type Plan = [Action]

aArgs :: Action -> [Object]
aArgs = snd

aName :: Action -> Name
aName = fst

class BoundedPlanner p where
    setBound :: p -> Int -> p

class (Domain d as p, Problem p) => ExternalPlanner ep d p as where
    makePlan :: ep -> d -> p -> IO (Maybe Plan)

class ActionSpecification as p => Domain d as p | d -> as where
    actionSpecification  :: d -> Name -> Maybe as
    actions              :: d -> [as]
    apply                :: d -> State -> Action -> Maybe State
    allApplicableActions :: Problem p => d -> p -> State -> [Action]

class Problem p where
    initialState :: p -> State
    isSolved     :: p -> State -> Bool
    objects      :: p -> [Object]

class Problem p => ActionSpecification a p | a -> p where
    name              :: a -> String
    arity             :: a -> Int
    isApplicable      :: a -> State -> [Name] -> Bool
    effect            :: a -> [Name] -> TupleSet GroundedPredicate
    applicableActions :: Problem p => p -> State -> a -> [Action]

-- applications :: (ActionSpecification as, Problem p) => p -> as -> [Action]
-- applications problem as =
--     map ((,) (name as)) $ replicateM (arity as) (objects problem)

-- applicableActions :: (ActionSpecification as, Problem p)
--                   => p -> State ->  as -> [Action]
-- applicableActions problem state as =
--     filter (isApplicable as state . aArgs) $ applications problem as

-- allApplicableActions :: (Domain dom as, Problem prob) => dom -> prob -> State -> [Action]
-- allApplicableActions  dom prob s =
--   let acts = actions dom
--   in concatMap (applicableActions prob s) acts
