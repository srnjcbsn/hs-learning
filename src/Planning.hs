module Planning where

import           Data.Set      (Set)
import           Control.Monad (liftM)
import           Data.TupleSet (TupleSet)
import           Data.Maybe    (fromMaybe)
import           Logic.Formula

type Object = String
type Name   = String
type Type   = String

type GroundedPredicate = Predicate Object
type State = Set GroundedPredicate
type TotalState = Set (Literal GroundedPredicate)

type Action = (Name, [Object])
type Plan = [Action]

-- | A state transition is a the old state, the action that was applied to that
--   state, and --- depending on the applicability of the action --- 'Just' an
--   an updated state with the actions effects applied, or 'Nothing'.
type Transition = (State, Action, State)

aArgs :: Action -> [Object]
aArgs = snd

aName :: Action -> Name
aName = fst

class BoundedPlanner p where
    setBound :: p -> Maybe Int -> p

class (Domain d p as, Problem p) => ExternalPlanner ep d p as where
    makePlan :: ep -> d -> p -> IO (Maybe Plan)

class (ActionSpecification as p, Eq d) => Domain d p as | d -> as where
    actionSpecification  :: d -> Name -> Maybe as
    actions              :: d -> [as]
    apply                :: d -> p -> State -> Action -> Maybe State
    allApplicableActions :: d -> p -> State -> [Action]
    isActionApplicable   :: d -> p -> State -> Action -> Bool
    isActionApplicable dom p s (aname,args) =
      fromMaybe False $ liftM (\as -> isApplicable p as s args)
                              (actionSpecification dom aname)

class Problem p where
    initialState :: p -> State
    isSolved     :: p -> State -> Bool
    objects      :: p -> [Object]
    setInitialState :: p -> State -> p


class Problem p => ActionSpecification as p | as -> p where
    name              :: as -> String
    arity             :: as -> Int
    isApplicable      :: p -> as -> State -> [Object] -> Bool
    effect            :: p -> as -> State -> [Name] -> TupleSet GroundedPredicate
    applicableActions :: p -> as -> State -> [Action]
