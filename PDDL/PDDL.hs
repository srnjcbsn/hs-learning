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
             deriving (Ord, Eq)

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

dom :: Domain
dom =
    let preds = Set.fromList [ ("at", ["l"])
                             , ("clean", ["l"])
                             ]

        precs l = Con (Set.fromList [ Predicate ("at", [l])
                                    , Neg (Predicate ("clean", [l]))
                                    ])

        effects l = Predicate ("clean", [l])
        suck = ("suck", ["l"], precs "l", effects "l")
      in Domain { predicates = preds
                , actionsSpecs = Set.fromList [suck]
                , constants = Set.empty
                }

type Planner = Domain -> Problem -> Maybe [Action]

apply :: Domain -> State -> Action -> State
apply = undefined

updateAction :: ActionSpec -> State -> State -> State -> ActionSpec
updateAction action oldState internalState actualState = undefined

showMultiple ls = unwords ls
showParameters ls = showMultiple $ map ("?" ++) ls

instance Show Formula where
    show (Predicate (n, vs)) =
        "(" ++ n ++ " " ++ showParameters vs ++ ")"
        -- unwords ["(" ++ n, map ((++) "?") vs ++ ")"]
    show (Neg f) = show f
    show (Con fs) = "(and " ++ showMultiple (map show (Set.toList fs)) ++ ")"

instance Show Domain where
    show domain =
        let boilerPlate = "(define (domain dom)\n\t(:requirements :strips)"
            consts = Set.toList (constants domain)
            constsStr = "(:constants" ++ showMultiple consts ++ ")"
            preds = Set.toList (predicates domain)
            showFluent (n, vs) = "(" ++ n ++ " " ++ showParameters vs ++ ")"
            predsStr = "(:predicates " ++ unwords (map showFluent preds) ++ ")"
            showActionS (n, vs, p, e) =
                "(:action " ++ n ++ "\n\t"
                ++ ":parameters (" ++ showParameters vs ++ ")\n\t"
                ++ ":precondition (" ++ show p ++ ")\n\t"
                ++ ":effect (" ++ show e ++ ")\n\t"
                ++ ")"
            actions = unwords $ map showActionS $ (Set.toList . actionsSpecs) domain
        in boilerPlate ++ constsStr ++ predsStr ++ actions ++ ")"
