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


import qualified Data.TupleSet as TSet
import Data.TupleSet (TupleSet)
import Planning

import           Data.List (find, intercalate)
import           Data.Set  (Set)
import qualified Data.Set  as Set

class ActionSpecification a => PDDLAction a where
    preConditions  :: a -> Formula
    effects        :: a -> Formula

data Argument = Const Name
              | Ref Name
              deriving (Show, Eq, Ord)

type FluentPredicate = (Name, [Argument])

data Formula = Predicate FluentPredicate
             | Neg Formula
             | Con [Formula]
             deriving (Ord, Eq, Show)

type PredicateSpec = (Name, [Name])

data ActionSpec = ActionSpec
    { asName    :: String
    , asParas   :: [Name]
    , asPrecond :: Formula
    , asEffect  :: Formula
    } deriving (Show, Eq, Ord)

type GroundedChanges = (Set GroundedPredicate, Set GroundedPredicate)

type GroundedAction = (GroundedChanges, GroundedChanges)

data PDDLDomain = PDDLDomain
    { dmName         :: Name
    , dmPredicates   :: [PredicateSpec]
    , dmActionsSpecs :: [ActionSpec]
    , dmConstants    :: [Name]
    } deriving (Show, Eq)

instance Domain PDDLDomain where
    actionSpecification = undefined
    actions             = undefined
    apply               = undefined

data PDDLProblem = PDDLProblem
    { probName         :: String
    , probObjs         :: [Object]
    , probDomain       :: String
    , probState        :: State
    , probGoal         :: Formula
    } deriving (Show, Eq)

instance Problem PDDLProblem where
    initialState = undefined
    isSolved     = undefined

-- | A state transition is a the old state, the action that was applied to that
--   state, and --- depending on the applicability of the action --- 'Just' an
--   an updated state with the actions effects applied, or 'Nothing'.
type Transition = (State, Action, Maybe State)

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
writeFluentPredicate (name, as) =
    "(" ++ name ++ " " ++ writeArgumentList as ++ ")"

writeGroundedPredicate :: GroundedPredicate -> String
writeGroundedPredicate (n, objs) =
    "(" ++ n ++ " " ++ unwords objs  ++ ")"

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

writeProblem :: PDDLProblem -> String
writeProblem prob =
    let defineStr = "(define (problem " ++ probName prob ++ ")"
        domStr    = "(:domain " ++ probDomain prob ++ ")"
        objsStr   = "(:objects " ++ unwords (probObjs prob) ++ ")"
        initStr   = "(:init " ++ writeState (probState prob) ++ ")"
        goalStr   = "(:goal " ++ writeFormula (probGoal prob) ++ ")"
    in intercalate "\n\t" [defineStr, domStr, objsStr, initStr, goalStr] ++ ")"


writeDomain :: PDDLDomain -> String
writeDomain domain =
    let defineStr = "(define (domain " ++ dmName domain ++ ")"
        reqsStr = "(:requirements :strips)"
        consts = dmConstants domain
        constsStr = "(:constants " ++ unwords consts ++ ")"
        preds = dmPredicates domain
        predsStr = "(:predicates " ++ unwords (map writePredicateSpec preds) ++ ")"
        actions = unwords $ map writeActionSpec $ dmActionsSpecs domain
    in intercalate "\n\t" [defineStr, reqsStr, constsStr, predsStr, actions] ++ ")"
