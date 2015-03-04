module Planning.PDDL
    (
    -- * Basic Types
      Name
    , Object
    , baseType

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
    , typeList

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
import qualified Data.Map      as Map
import           Data.Maybe    (fromMaybe)
import           Data.Set      (Set)
import qualified Data.Set      as Set

data Argument = Const Name
              | Ref Name
              deriving (Show, Eq, Ord)

type FluentPredicate = Predicate Argument
type PredicateSpec = Predicate (Name, Type)

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

type GroundedAction = (GrFormula, GroundedChanges)

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

baseType :: Type
baseType = "object"

typeList :: ActionSpec -> [(Name, Type)]
typeList aSpec = zip (asParas aSpec)
               $ map (fromMaybe baseType . flip Map.lookup (asTypes aSpec))
               $ asParas aSpec

data PDDLGraph = PDDLGraph (PDDLDomain, PDDLProblem)



pName :: FluentPredicate -> Name
pName = predName

pArgs :: FluentPredicate -> [Argument]
pArgs = predArgs

paramNames :: PredicateSpec -> [Name]
paramNames (Predicate _ params) = map fst params

-- | Returns the action specification with the given name in the domain,
--   or 'Nothing' if it could not be found.
actionSpec :: PDDLDomain -> Name -> Maybe ActionSpec
actionSpec domain n = find ((== n) . asName) (dmActionsSpecs domain)

writeState :: State -> String
writeState state =
    unwords $ map writeGroundedPredicate $ Set.toList state

writeArgument :: Argument -> String
writeArgument (Ref r)   = "?" ++ r
writeArgument (Const c) = c

writeArgumentList :: [Argument] -> String
writeArgumentList as = unwords (map writeArgument as)

-- writeParameterList :: [String] -> String
-- writeParameterList ps = writeArgumentList $ map Ref ps

writeTypedList :: (Name -> String) -> [(Name, Type)] -> String
writeTypedList w ts@((_, t) : _) = sameList ++ " - " ++ t ++ " " ++ rest
    where sameList = unwords $ map (w . fst) same
          (same, different) = span ((t ==) . snd) ts
          rest = writeTypedList w different
writeTypedList _ [] = ""

writeTypedParameterList :: [(Name, Type)] -> String
writeTypedParameterList = writeTypedList (writeArgument . Ref)

writeFluentPredicate :: FluentPredicate -> String
writeFluentPredicate (Predicate pname as) =
    "(" ++ pname ++ " " ++ writeArgumentList as ++ ")"

writeGroundedPredicate :: GroundedPredicate -> String
writeGroundedPredicate (Predicate pname objs) =
    "(" ++ pname ++ " " ++ unwords objs  ++ ")"

writePredicateSpec :: PredicateSpec -> String
writePredicateSpec (Predicate pname ps) =
    "(" ++ pname ++ " " ++ writeTypedParameterList ps ++ ")"

writeActionSpec :: ActionSpec -> String
writeActionSpec as =
    "(:action " ++ asName as
    ++ "\t:parameters (" ++ params ++ ")\n"
    ++ "\t:precondition " ++ precond ++ "\n"
    ++ "\t:effect " ++ eff ++ "\n"
    ++ ")"
        where params  = writeTypedParameterList (Map.toList (asTypes as))
              precond = writeUngrFormula (asPrecond as)
              eff     = writeUngrFormula (asEffect as)

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
        objs      = writeTypedList id $ Map.toList (probTypes prob)
        objsStr   = "(:objects " ++ objs ++ ")"
        initStr   = "(:init " ++ writeState (probState prob) ++ ")"
        goalStr   = "(:goal " ++ writeGrFormula (probGoal prob) ++ ")"
    in intercalate "\n\t" [defineStr, domStr, objsStr, initStr, goalStr] ++ ")"


writeDomain :: PDDLDomain -> String
writeDomain domain =
    let defineStr = "(define (domain " ++ dmName domain ++ ")"
        reqsStr   = "(:requirements :strips :typing)"
        typesStr  = "(:types " ++ unwords (dmTypes domain) ++ ")"
        consts    = dmConstants domain
        constsStr = "(:constants " ++ unwords consts ++ ")"
        preds     = dmPredicates domain
        predsStr  = "(:predicates " ++ unwords (map writePredicateSpec preds) ++ ")"
        aSpecs    = unwords $ map writeActionSpec $ dmActionsSpecs domain
    in intercalate "\n\t" [ defineStr
                          , reqsStr
                          , typesStr
                          , constsStr
                          , predsStr
                          , aSpecs
                          ] ++ ")"
