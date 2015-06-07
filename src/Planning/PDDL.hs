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
    , pName
    , pArgs
    , ePos
    , eNeg
    , gPos
    , gNeg

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
    , GroundedEffects
    , GroundedAction
    , State
    , Plan

    -- ** Actions
    , Action
    , aName
    , aArgs

    ) where

import           Logic.Formula
import           Planning      as Plng

import           Data.List     (find)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe    (fromMaybe)
import           Data.Set      (Set)
-- import qualified Data.Set      as Set

type FluentPredicate = Predicate Term
type PredicateSpec = Predicate (Name, Type)
type GroundedAction = (Bool, GroundedEffects)

type GroundedEffects = (Set GroundedPredicate, Set GroundedPredicate)

type Variable = String

data Term = TName Name
          | TVar  Variable
          deriving (Eq, Ord, Show)


data GoalDesc = GAnd [GoalDesc]
              | GLit (LitPred Term)
              | GOr  [GoalDesc]
              | GNot GoalDesc
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


ePos :: Predicate Term -> Effect
ePos = ELit . Pos

eNeg :: Predicate Term  -> Effect
eNeg = ELit . Neg

gPos :: Predicate Term  -> GoalDesc
gPos = GLit . Pos

gNeg :: Predicate Term  -> GoalDesc
gNeg = GLit . Neg

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
