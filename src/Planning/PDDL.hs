module Planning.PDDL
    (
    -- * Basic Types
      Name
    , Object
    , baseType
    , AllPossibleObjects

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
    , Context
    , instantiateTerm
    , envSpecToProblem
    , allObjsToProblem

    -- * Grounded data
    , GroundedPredicate
    , GroundedEffects
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

type GroundedEffects = (Set GroundedPredicate, Set GroundedPredicate)
type AllPossibleObjects = [Object]
type Variable = String

data Term = TName Name
          | TVar  Variable
          deriving (Eq, Ord, Show)

type Context = (Map Variable Object, AllPossibleObjects)

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
    { asName      :: String
    , asParas     :: [Name]
    -- , asPrecond    :: Formula Argument
    , asPrecond   :: GoalDesc
    -- , asEffect     :: Formula Argument
    , asEffect    :: Effect
    , asConstants :: [Name]
    , asTypes     :: Map Name Type
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
    , probObjs   :: AllPossibleObjects
    , probDomain :: String
    , probState  :: State
    , probGoal   :: GoalDesc
    , probTypes  :: Map Name Type
    } deriving (Show, Eq)

data PDDLEnvSpec = PDDLEnvSpec
    { envsPredSpecs :: [PredicateSpec]
    , envsConsts    :: [Name]
    , envsObjs      :: AllPossibleObjects
    , envsObjTypes  :: Map Name Type
    } deriving (Show, Eq)

data PDDLGraph = PDDLGraph (PDDLDomain, PDDLProblem)

instantiateTerm :: Context -> Term -> Object
instantiateTerm (tc,_) (TVar t) =
  fromMaybe (error "variable " ++ show t ++ " don't exist")
            $ Map.lookup t tc
instantiateTerm _ (TName t) = t

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

envSpecToProblem :: PDDLEnvSpec -> PDDLProblem
envSpecToProblem envs = PDDLProblem
  { probName   = error "call to probName failed: Fake envspec problem"
  , probObjs   = envsObjs envs
  , probDomain = error "call to probDomain failed: Fake envspec problem"
  , probState  = error "call to probState failed: Fake envspec problem"
  , probGoal   = error "call to probGoal failed: Fake envspec problem"
  , probTypes  =  envsObjTypes envs
  }

allObjsToProblem :: AllPossibleObjects -> PDDLProblem
allObjsToProblem allobjs = PDDLProblem
  { probName   = error "call to probName failed: Fake envspec problem"
  , probObjs   = allobjs
  , probDomain = error "call to probDomain failed: Fake envspec problem"
  , probState  = error "call to probState failed: Fake envspec problem"
  , probGoal   = error "call to probGoal failed: Fake envspec problem"
  , probTypes  =  error "call to probTypes failed: Fake envspec problem"
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
unsActionSpec domain n =
  fromMaybe
    (error $ "ansActionSpec: could not find action spec with name "
           ++ n ++ " in domain.")
    (actionSpec domain n)
