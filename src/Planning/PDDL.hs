module Planning.PDDL
    (
    -- * Basic Types
      Name
    , Object
    , baseType

    -- * Formulae
    , FluentPredicate
    , Variable
    , Term (..)
    , LitPred (..)
    , pName
    , pArgs

    -- * Composite types
    , GoalDesc (..)
    , Effect (..)
    , PDDLDomain (..)
    , PDDLProblem (..)
    , PredicateSpec
    , ActionSpec (..)
    , PDDLEnvSpec (..)
    , pddlEnvSpec
    , paramNames
    , actionSpec
    , unsActionSpec
    , PDDLGraph(..)
    , typeList

    -- * Grounded data
    , GroundedPredicate
    , GroundedChanges
    -- , GroundedAction
    , State
    , Plan

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

-- data Argument = Const Name
--               | Ref Name
--               deriving (Show, Eq, Ord)

type FluentPredicate = Predicate Term
type PredicateSpec = Predicate (Name, Type)

-- type GrFormula = Formula Name
-- type UngrFormula = Formula Argument

type GroundedChanges = (Set GroundedPredicate, Set GroundedPredicate)
-- type GroundedAction = (GrFormula, GroundedChanges)

type Variable = String

data Term = TName Name
          | TVar  Variable
          deriving (Eq, Ord, Show)

data LitPred a = Pos (Predicate a)
               | Neg (Predicate a)  
               deriving (Eq, Ord, Show)

data GoalDesc = GAnd [GoalDesc]
              | GLit (LitPred Term)
              | GOr  [GoalDesc]
              | GNOt [GoalDesc]
              deriving (Eq, Ord, Show)

data Effect = EAnd [Effect]
            | ELit (LitPred Term)
            | EForall [Variable] Effect
            | EWhen GoalDesc Effect
            deriving (Eq, Ord, Show)

data ActionSpec = ActionSpec
    { asName       :: String
    , asParas      :: [Name]
    -- , asPrecond    :: Formula Argument
    , asPrecond    :: GoalDesc
    -- , asEffect     :: Formula Argument
    , asEffect     :: Effect
    , asConstants  :: [Name]
    , asTypes      :: Map Name Type
    } deriving (Show, Eq, Ord)

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
    , probGoal   :: GoalDesc
    , probTypes  :: Map Name Type
    } deriving (Show, Eq)

data PDDLEnvSpec = PDDLEnvSpec
    { envsPredSpecs :: [PredicateSpec]
    , envsConsts    :: [Name]
    , envsObjs      :: [Object]
    , envsObjTypes  :: Map Name Type
    }

data PDDLGraph = PDDLGraph (PDDLDomain, PDDLProblem)

pddlEnvSpec :: PDDLDomain -> PDDLProblem -> PDDLEnvSpec
pddlEnvSpec dom prob = PDDLEnvSpec 
    { envsPredSpecs = dmPredicates dom
    , envsConsts    = dmConstants dom
    , envsObjs      = probObjs prob
    , envsObjTypes  = probTypes prob
    }

baseType :: Type
baseType = "object"

typeList :: ActionSpec -> [(Name, Type)]
typeList aSpec = zip (asParas aSpec)
               $ map (fromMaybe baseType . flip Map.lookup (asTypes aSpec))
               $ asParas aSpec


pName :: FluentPredicate -> Name
pName = predName

pArgs :: FluentPredicate -> [Term]
pArgs = predArgs

paramNames :: PredicateSpec -> [Name]
paramNames (Predicate _ params) = map fst params

-- | Returns the action specification with the given name in the domain,
--   or 'Nothing' if it could not be found.
actionSpec :: PDDLDomain -> Name -> Maybe ActionSpec
actionSpec domain n = find ((== n) . asName) (dmActionsSpecs domain)

-- | Find the action specification with the given name in the given domain.
--   Throws an error if the action specification could not be found.
unsActionSpec :: PDDLDomain -> Name -> ActionSpec
unsActionSpec domain n = case actionSpec domain n of
    Just as -> as
    Nothing -> error $  "ansActionSpec: could not find action spec with name "
                     ++ n ++ " in domain."

writeState :: State -> String
writeState state =
    unwords $ map writeGroundedPredicate $ Set.toList state

writeArgument :: Term -> String
writeArgument (TVar r)   = "?" ++ r
writeArgument (TName c) = c

writeArgumentList :: [Term] -> String
writeArgumentList as = unwords (map writeArgument as)

writeParameterList :: [String] -> String
writeParameterList ps = writeArgumentList $ map TVar ps

writeTypedList :: (Name -> String) -> [(Name, Type)] -> String
writeTypedList w ts@((_, t) : _) = sameList ++ " - " ++ t ++ " " ++ rest
    where sameList = unwords $ map (w . fst) same
          (same, different) = span ((t ==) . snd) ts
          rest = writeTypedList w different
writeTypedList _ [] = ""

writeTypedParameterList :: [(Name, Type)] -> String
writeTypedParameterList = writeTypedList (writeArgument . TVar)

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
              precond = writeGoalDescription (asPrecond as)
              eff     = writeEffect (asEffect as)

writeGoalDescription :: GoalDesc -> String 
writeGoalDescription = undefined

writeEffect :: Effect -> String
writeEffect = undefined

writeProblem :: PDDLProblem -> String
writeProblem prob =
    let defineStr = "(define (problem " ++ probName prob ++ ")"
        domStr    = "(:domain " ++ probDomain prob ++ ")"
        objs      = writeTypedList id $ Map.toList (probTypes prob)
        objsStr   = "(:objects " ++ objs ++ ")"
        initStr   = "(:init " ++ writeState (probState prob) ++ ")"
        goalStr   = "(:goal " ++ writeGoalDescription (probGoal prob) ++ ")"
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
