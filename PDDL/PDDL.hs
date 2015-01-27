module PDDL where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)

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

showParameters :: [String] -> String
showParameters ls = unwords $ map ("?" ++) ls

class PDDL p where
    toPddl :: p -> String

instance PDDL Formula where
    toPddl (Predicate (n, vs)) =
        "(" ++ n ++ " " ++ showParameters vs ++ ")"
    toPddl (Neg f) = "(not " ++ toPddl f ++ ")"
    toPddl (Con fs) = "(and " ++ unwords (map toPddl (Set.toList fs)) ++ ")"

instance PDDL Domain where
    toPddl domain =
        let consts = Set.toList (constants domain)
            constsStr = "(:constants" ++ unwords consts ++ ")"
            preds = Set.toList (predicates domain)
            showFluent (n, vs) = "(" ++ n ++ " " ++ showParameters vs ++ ")"
            predsStr = "(:predicates " ++ unwords (map showFluent preds) ++ ")"
            showActionS (n, vs, p, e) =
                "(:action " ++ n ++ "\n\t"
                ++ ":parameters (" ++ showParameters vs ++ ")\n\t"
                ++ ":precondition (" ++ toPddl p ++ ")\n\t"
                ++ ":effect (" ++ toPddl e ++ ")\n\t"
                ++ ")"
            actions = unwords $ map showActionS $ (Set.toList . actionsSpecs) domain
        in intercalate "\n\t" [constsStr, predsStr, actions]

domainToPdll :: String -> Domain -> String
domainToPdll name domain =
    "(define (domain " ++ name ++ ")\n\t(:requirements :strips)\n\t"
    ++ toPddl domain ++ "\n\t)"
