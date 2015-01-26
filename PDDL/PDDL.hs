module PDDL where

import Data.Set (Set)
import qualified Data.Set as Set

type Name = String

type Variable = Name
type Fluent = (Name, [Variable])
type GroundedFluent = (Name, [Name])

data Formula = Predicate Fluent
             | Neg Formula
             | Con (Set Formula)
             deriving (Ord, Eq, Show)

type ActionSpec = (Name, [Name], Formula, Formula)

type Action = (Name, [Name])

data Domain = Domain
    { predicates   :: Set Fluent
    , actionsSpecs :: Set ActionSpec
    , constants    :: Set Name
    }

type State = Set GroundedFluent

data Problem = Problem
    { initialState :: State
    , goalState    :: State
    }

dom =
    let preds = Set.fromList [ Predicate ("at", ["l"])
                             , Predicate ("clean", ["l"])
                             ]

        precs l = Con (Set.fromList [ Predicate ("at", [l])
                                    , Neg (Predicate ("clean", [l]))
                                    ])

        effects l = Predicate ("clean", [l])
        suck = ("suck", ["l"], precs "l", effects "l")
      in suck

type Planner = Domain -> Problem -> Maybe [Action]

apply :: Domain -> State -> Action -> State
apply = undefined

updateAction :: ActionSpec -> State -> State -> State -> ActionSpec
updateAction action oldState internalState actualState = undefined
