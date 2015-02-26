module Planning.PDDL
    (
    -- * Basic Types
      Name
    , Object

    -- * Formulae
    , Formula (..)
    , FluentPredicate
    , Argument (Const, Ref)
    , pName
    , pArgs

    -- * Composite types
    , PDDLDomain (..)
    , PDDLProblem (..)
    , PredicateSpec
    , ActionSpec (..)
    , paramNames
    , actionSpec
    , PDDLGraph(..)

    -- * Grounded data
    , GroundedPredicate
    , GroundedChanges
    , GroundedAction
    , State
    , Plan
    , Transition

    -- ** Actions
    , Action
    , aName
    , aArgs

    -- * Functions for converting PDDL types to 'String's
    , writeDomain
    , writeProblem
    ) where

import           Logic.Formula
import           Planning      as Plng

import           Data.List     (find, intercalate)
import           Data.Map      (Map)
import           Data.Set      (Set)
import qualified Data.Set      as Set

-- class ActionSpecification a => PDDLAction a where
--     preConditions  :: a -> Formula
--     effects        :: a -> Formula

data Argument = Const Name
              | Ref Name
              deriving (Show, Eq, Ord)

-- type FluentPredicate = (Name, [Argument])
type FluentPredicate = Predicate Argument

-- data Formula = Predicate FluentPredicate
--              | Neg Formula
--              | Con [Formula]
--              deriving (Ord, Eq, Show)

-- type PredicateSpec = (Name, [Name])
type PredicateSpec = Predicate Name

type GrFormula = Formula Name

type UngrFormula = Formula Argument

data ActionSpec = ActionSpec
    { asName      :: String
    , asParas     :: [Name]
    , asPrecond   :: Formula Argument
    , asEffect    :: Formula Argument
    , asConstants :: [Name]
    , asTypes     :: Map Name Type
    } deriving (Show, Eq, Ord)

type GroundedChanges = (Set GroundedPredicate, Set GroundedPredicate)

type GroundedAction = (GroundedChanges, GroundedChanges)

data PDDLDomain = PDDLDomain
    { dmName         :: Name
    , dmPredicates   :: [PredicateSpec]
    , dmActionsSpecs :: [ActionSpec]
    , dmConstants    :: [Name]
    , dmTypes        :: [Type]
    } deriving (Show, Eq)

data PDDLProblem = PDDLProblem
    { probName   :: String
    , probObjs   :: [Object]
    , probDomain :: String
    , probState  :: State
    , probGoal   :: Formula Name
    , probTypes  :: Map Name Type
    } deriving (Show, Eq)

data PDDLGraph = PDDLGraph (PDDLDomain, PDDLProblem)

-- | A state transition is a the old state, the action that was applied to that
--   state, and --- depending on the applicability of the action --- 'Just' an
--   an updated state with the actions effects applied, or 'Nothing'.
type Transition = (State, Action, Maybe State)

-- | Checks if the preconditions of a grounded action are satisfied
isActionValid :: State -> GroundedAction -> Bool
isActionValid s ((posCond, negCond), _) =
  Set.isSubsetOf posCond s &&
  Set.null (Set.intersection negCond s)

pName :: FluentPredicate -> Name
pName (Predicate name _) = name

pArgs :: FluentPredicate -> [Argument]
pArgs (Predicate _ args) = args

paramNames :: PredicateSpec -> [Name]
paramNames (Predicate _ params) = params

-- | Returns the action specification with the given name in the domain,
--   or 'Nothing' if it could not be found.
actionSpec :: PDDLDomain -> Name -> Maybe ActionSpec
actionSpec domain name = find ((== name) . asName) (dmActionsSpecs domain)

writeState :: State -> String
writeState state =
    unwords $ map writeGroundedPredicate $ Set.toList state

writeArgument :: Argument -> String
writeArgument (Ref r)   = "?" ++ r
writeArgument (Const c) = c

writeArgumentList :: [Argument] -> String
writeArgumentList as = unwords (map writeArgument as)

writeParameterList :: [String] -> String
writeParameterList ps = writeArgumentList $ map Ref ps

writeFluentPredicate :: FluentPredicate -> String
writeFluentPredicate (Predicate name as) =
    "(" ++ name ++ " " ++ writeArgumentList as ++ ")"

writeGroundedPredicate :: GroundedPredicate -> String
writeGroundedPredicate (Predicate name objs) =
    "(" ++ name ++ " " ++ unwords objs  ++ ")"

writePredicateSpec :: PredicateSpec -> String
writePredicateSpec (Predicate name ps) =
    "(" ++ name ++ " " ++ writeArgumentList (map Ref ps) ++ ")"

writeActionSpec :: ActionSpec -> String
writeActionSpec as =
    "(:action " ++ asName as
    ++ "\t:parameters (" ++ writeParameterList (asParas as) ++ ")\n"
    ++ "\t:precondition " ++ writeUngrFormula (asPrecond as) ++ "\n"
    ++ "\t:effect " ++ writeUngrFormula (asEffect as) ++ "\n"
    ++ ")"

writeUngrFormula :: UngrFormula -> String
writeUngrFormula (Pred p) = writeFluentPredicate p
writeUngrFormula (Neg f) = "(not " ++ writeUngrFormula f ++ ")"
writeUngrFormula (Con fs) = "(and " ++ unwords (map writeUngrFormula fs) ++ ")"

writeGrFormula :: GrFormula -> String
writeGrFormula (Pred p) = writeGroundedPredicate p
writeGrFormula (Neg f) = "(not " ++ writeGrFormula f ++ ")"
writeGrFormula (Con fs) = "(and " ++ unwords (map writeGrFormula fs) ++ ")"

writeProblem :: PDDLProblem -> String
writeProblem prob =
    let defineStr = "(define (problem " ++ probName prob ++ ")"
        domStr    = "(:domain " ++ probDomain prob ++ ")"
        objsStr   = "(:objects " ++ unwords (probObjs prob) ++ ")"
        initStr   = "(:init " ++ writeState (probState prob) ++ ")"
        goalStr   = "(:goal " ++ writeGrFormula (probGoal prob) ++ ")"
    in intercalate "\n\t" [defineStr, domStr, objsStr, initStr, goalStr] ++ ")"


writeDomain :: PDDLDomain -> String
writeDomain domain =
    let defineStr = "(define (domain " ++ dmName domain ++ ")"
        reqsStr   = "(:requirements :strips)"
        consts    = dmConstants domain
        constsStr = "(:constants " ++ unwords consts ++ ")"
        preds     = dmPredicates domain
        predsStr  = "(:predicates " ++ unwords (map writePredicateSpec preds) ++ ")"
        aSpecs    = unwords $ map writeActionSpec $ dmActionsSpecs domain
    in intercalate "\n\t" [defineStr, reqsStr, constsStr, predsStr, aSpecs] ++ ")"
