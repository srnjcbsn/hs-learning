module PDDL.Type where

import Data.List (intercalate, find)
import           Data.Set  (Set)
import qualified Data.Set  as Set

type Name = String
type Type = String
type Object  = String

data Argument = Const Name
              | Ref Name
              deriving (Show, Eq, Ord)

type FluentPredicate = (Name, [Argument])

data Formula = Predicate FluentPredicate
             | Neg (Formula)
             | Con [Formula]
             deriving (Ord, Eq, Show)

type PredicateSpec = (Name, [Name])

type Parameters = [Name]

data ActionSpec = ActionSpec
    { asName   :: String
    , asParas  :: Parameters
    , asPrecond :: Formula
    , asEffect  :: Formula
    } deriving (Show, Eq)

type Arguments = [Object]

type Action = (Name, Arguments)

type GroundedPredicate = (Name, Arguments)

type GroundedChanges = (Set GroundedPredicate, Set GroundedPredicate)

type GroundedAction = (GroundedChanges, GroundedChanges)

data Domain = Domain
    { dmName         :: Name
    , dmPredicates   :: [PredicateSpec]
    , dmActionsSpecs :: [ActionSpec]
    , dmConstants    :: [Name]
    } deriving (Show, Eq)

type State = Set GroundedPredicate

data Problem = Problem
    { probName         :: String
    , probInitialState :: State
    , probGoalState    :: State
    } deriving (Show, Eq)

pName :: FluentPredicate -> Name
pName = fst

pArgs :: FluentPredicate -> [Argument]
pArgs = snd


aArgs :: Action -> [Object]
aArgs = snd

aName :: Action -> Name
aName = fst

paramNames :: PredicateSpec -> [Name]
paramNames = snd

actionSpec :: Domain -> Name -> Maybe ActionSpec
actionSpec domain name = find ((== name) . asName) (dmActionsSpecs domain)


dom :: Domain
dom =
    Domain { dmName         = "vacuum"
           , dmPredicates   = preds
           , dmActionsSpecs = [suck]
           , dmConstants    = []
           } where
                preds = [ ("at", ["l"])
                        , ("clean", ["l"])
                        ]

                precs l = Con [ Predicate ("at", [l])
                              , Neg (Predicate ("clean", [l]))
                              ]

                effects l = Predicate ("clean", [l])
                suck = ActionSpec { asName = "suck"
                                  , asParas = ["l"]
                                  , asPrecond = precs (Ref "l")
                                  , asEffect = effects (Ref "l")
                                  }

type Planner = Domain -> Problem -> Maybe [Action]

updateAction :: ActionSpec -> State -> State -> State -> ActionSpec
updateAction action oldState internalState actualState = undefined

-- showParameters :: [String] -> String
-- showParameters ls = unwords $ map ("?" ++) ls

writeArgument :: Argument -> String
writeArgument (Ref r)   = "?" ++ r
writeArgument (Const c) = c

writeArgumentList :: [Argument] -> String
writeArgumentList as = intercalate " " (map writeArgument as)

writeParameterList :: [String] -> String
writeParameterList ps = writeArgumentList $ map Ref ps

writeFluentPredicate :: FluentPredicate -> String
writeFluentPredicate (name, as) =
    "(" ++ name ++ " " ++ writeArgumentList as ++ ")"

writePredicateSpec :: PredicateSpec -> String
writePredicateSpec (name, ps) =
    "(" ++ name ++ " " ++ writeArgumentList (map Ref ps) ++ ")"

writeActionSpec :: ActionSpec -> String
writeActionSpec as =
    "(:action " ++ asName as
    ++ "\t:parameters (" ++ writeParameterList (asParas as) ++ ")\n"
    ++ "\t:precondition " ++ writeFormula (asPrecond as) ++ "\n"
    ++ "\t:effect " ++ writeFormula (asEffect as) ++ "\n"
    ++ ")"

writeFormula :: Formula -> String
writeFormula (Predicate f) = writeFluentPredicate f
writeFormula (Neg f) = "(not " ++ writeFormula f ++ ")"
writeFormula (Con fs) = "(and " ++ unwords (map writeFormula fs) ++ ")"

writeDomain :: Domain -> String
writeDomain domain =
    let defineStr = "(define (domain " ++ dmName domain ++ ")"
        reqsStr = "(:requirements :strips)"
        consts = dmConstants domain
        constsStr = "(:constants " ++ unwords consts ++ ")"
        preds = dmPredicates domain
        predsStr = "(:predicates " ++ unwords (map writePredicateSpec preds) ++ ")"
        actions = unwords $ map writeActionSpec $ dmActionsSpecs domain
    in intercalate "\n\t" [defineStr, reqsStr, constsStr, predsStr, actions] ++ ")"

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
