module PDDL where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)

type Name = String

type Variable = Name
newtype Fluent = Fluent (Name, [Variable]) deriving (Show, Ord, Eq)
type GroundedFluent = (Name, [Name])

data Formula = Predicate Fluent
             | Neg Formula
             | Con (Set Formula)
             deriving (Ord, Eq, Show)

data ActionSpec = ActionSpec
    { name :: String
    , variables :: [Variable]
    , precond :: Formula
    , effect :: Formula
    } deriving Show

type Action = (Name, [Name])

data Domain = Domain
    { predicates   :: Set Fluent
    , actionsSpecs :: [ActionSpec]
    , constants    :: Set Name
    } deriving Show

type State = Set GroundedFluent

data Problem = Problem
    { initialState :: State
    , goalState    :: State
    }

dom :: Domain
dom =
    Domain { predicates = preds
    , actionsSpecs = [suck]
    , constants = Set.empty
    }
    where
        preds = Set.fromList [ Fluent ("at", ["l"])
                             , Fluent ("clean", ["l"])
                             ]

        precs l = Con (Set.fromList [ Predicate $ Fluent ("at", [l])
                                    , Neg (Predicate $ Fluent ("clean", [l]))
                                    ])

        effects l = Predicate $ Fluent ("clean", [l])
        suck = ActionSpec { name = "suck"
                          , variables = ["l"]
                          , precond = precs "l"
                          , effect = effects "l"
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

-- instance PDDL ActionSpec where
--     toPDDL as =
--         "(" ++ name as
--         ++ "\t:parameters (" ++ showParameters (variables as) ++ ")\n"
--         ++ "\t:precondition (" ++ toPddl (precond as) ++ ")\n"
--         ++ "\t:effect (" ++ toPddl (effect as) ++ ")\n"
--         ++ ")"

-- instance PDDL Fluent where
--     toPddl (Fluent (name, vs)) =
--         "(" ++ name ++ " " ++ showParameters vs ++ ")"
--
-- instance PDDL Formula where
--     toPddl (Predicate f) = toPddl f
--     toPddl (Neg f) = "(not " ++ toPddl f ++ ")"
--     toPddl (Con fs) = "(and " ++ unwords (map toPddl (Set.toList fs)) ++ ")"
--
-- instance PDDL Domain where
--     toPddl domain =
--         let consts = Set.toList (constants domain)
--             constsStr = "(:constants" ++ unwords consts ++ ")"
--             preds = Set.toList (predicates domain)
--             predsStr = "(:predicates " ++ unwords (map toPddl preds) ++ ")"
--             showActionS (n, vs, p, e) =
--                 "(:action " ++ n ++ "\n\t"
--                 ++ ":parameters (" ++ showParameters vs ++ ")\n\t"
--                 ++ ":precondition (" ++ toPddl p ++ ")\n\t"
--                 ++ ":effect (" ++ toPddl e ++ ")\n\t"
--                 ++ ")"
--             actions = unwords $ map showActionS $ (Set.toList . actionsSpecs) domain
--         in intercalate "\n\t" [constsStr, predsStr, actions]
--
-- domainToPdll :: String -> Domain -> String
-- domainToPdll name domain =
--     "(define (domain " ++ name ++ ")\n\t(:requirements :strips)\n\t"
--     ++ toPddl domain ++ "\n\t)"
