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
    , TotalState
    , Plan
    , allGroundedPredicates
    , notInState
    , totalState

    -- ** Actions
    , Action
    , aName
    , aArgs

    -- ** Other
    , apply
    , findActionSpec
    , instantiateAction
    , ground
    , applyEffects
    , isSatisfied
    , contextFromDom
    , contextFromAs
    , context
    , groundMany
    , isActionSatisfied

    ) where

import           Graph
import           Graph.Search
import           Logic.Formula
import           Planning       as Plng

import           Control.Arrow  ((***))
import           Control.Monad  (replicateM)
import           Data.List      (find, intercalate)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (fromMaybe)
import           Data.Set       (Set, (\\))
import qualified Data.Set       as Set
import           Data.Tuple
import qualified Data.TupleSet  as TSet
import           Data.UnsafeMap


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
-- | Construct all possible predicates given predicate specification from a
--   domain and objects from a problem.
allGroundedPredicates :: PDDLEnvSpec -> Set GroundedPredicate
allGroundedPredicates eSpec =
    Set.fromList $ concatMap fromSpec $ envsPredSpecs eSpec where
        fromSpec :: PredicateSpec -> [GroundedPredicate]
        fromSpec (Predicate pn pa) = [ Predicate pn p
                                     | p <- replicateM (length pa) (envsObjs eSpec)
                                     ]

-- | Constructs a set of grounded predicates not occuring in the given state
notInState :: State -> PDDLEnvSpec -> State
notInState s eSpec = allGroundedPredicates eSpec \\ s

totalState :: State -> PDDLEnvSpec -> TotalState
totalState s eSpec = Set.map Pos s `Set.union` Set.map Neg ns where
    ns = notInState s eSpec


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



-- | Finds the action spec of an action in a domain
findActionSpec :: PDDLDomain -> Action -> ActionSpec
findActionSpec domain (n, _) = case actionSpec domain n of
    Just aSpec -> aSpec
    Nothing    ->
        error $  "Action specification with name " ++ n
              ++ " does not exist in domain (action names: "
              ++ names ++ ")"
        where names = intercalate ", "
                    $ map asName (dmActionsSpecs domain)

-- | Applies the grounded actions to a state, if the action is not valid nothing is returned
applyEffects :: State -> GroundedEffects -> State
applyEffects s (posEff,negEff) = Set.union (Set.difference s negEff) posEff


-- | Instantiates a formula into the actual positive and negative changes
instantiateAction :: AllPossibleObjects -> ActionSpec -> State -> Action ->  GroundedEffects
instantiateAction espec as s a =
  instantiateEffects (contextFromAs espec as a) s (asEffect as)

instantiateEffects :: Context -> State -> Effect -> GroundedEffects
instantiateEffects c s (EAnd l) = TSet.unions $ map (instantiateEffects c s ) l
instantiateEffects c s (EForall vars (EWhen gd e)) = effects where
  contexts = satisfyingContexts c vars s gd
  effects = TSet.unions $ map (flip (`instantiateEffects` s) e) contexts
instantiateEffects c@(_,allobjs) s (EForall vars e) = effects where
  objsForVars = replicate (length vars) allobjs
  contexts = contextsSequences c vars objsForVars
  effects = TSet.unions $ map (flip (`instantiateEffects` s) e) contexts
instantiateEffects c s (EWhen gd e)
    | isSatisfied c s gd = instantiateEffects c s e
    | otherwise = TSet.empty
instantiateEffects c _ (ELit (Neg p)) = (Set.empty, Set.singleton $ ground c p)
instantiateEffects c _ (ELit (Pos p)) = (Set.singleton $ ground c p, Set.empty)

contextsSequences :: Context
                  -> [Variable]
                  -> [[Object]]
                  -> [Context]
contextsSequences (m,allobjs) vars objsForVars = contexts where
  contextObjs = sequence objsForVars
  mkContextMap objs = Map.union (Map.fromList (zip vars objs)) m
  contexts = [ (mkContextMap objs, allobjs) | objs <- contextObjs]


satisfyingContexts :: Context
                   -> [Variable]
                   -> State
                   -> GoalDesc
                   -> [Context]
satisfyingContexts c vars s gd = contextsSequences c vars combis where
  mapCombi = satisfyingCombi c vars s gd
  errorMsg = "logic:satisfyingContexts can't find variable, "
           ++"rember planning with action variables in condition effects not supported"
  combis   = map (Set.toList . flip (unsLookup errorMsg) mapCombi) vars


-- | finds all variable combinations that satisfies a goaldesc
--    For instance if:
--      GD    = p(x,y) GAnd p(z,x)
--      State = p(1,2), p(3,1)
--    Gives: x = {1}, y = {2}, z = {1,3}
satisfyingCombi :: Context
                -> [Variable]
                -> State
                -> GoalDesc
                -> Map Variable (Set Object)
satisfyingCombi c vars s (GAnd gds) =
  intersecCombi $ map (satisfyingCombi c vars s) gds
satisfyingCombi c vars s (GOr gds) =
  unionCombi $ map (satisfyingCombi c vars s) gds
satisfyingCombi c vars _ (GNot _) = initVarCombi c vars -- Too hard (resets)
satisfyingCombi (_,allobjs) _ s (GLit lit) = varsMap where
  Predicate n args = atom lit
  relS = Set.filter ((== n) . predName) s
  relArgs = Set.map predArgs relS
  allArgs = Set.fromList $ replicateM (length args) allobjs
  useArgs = Set.toList
          $ case lit of
              Pos _ -> relArgs
              Neg _ -> allArgs \\ relArgs
  emptyArgSets = replicate (length args) Set.empty
  stackedArgs = foldr (zipWith Set.insert) emptyArgSets useArgs

  combin = zip args stackedArgs
  varOnlyCombi = [(var,objs) | (TVar var, objs) <- combin]
  varsMap = Map.fromListWith Set.union varOnlyCombi


unionCombi ::  [Map Variable (Set Object)]
           ->  Map Variable (Set Object)
unionCombi = Map.unionsWith Set.union

intersecCombi ::  [Map Variable (Set Object)]
              ->  Map Variable (Set Object)
intersecCombi = Map.unionsWith Set.intersection

initVarCombi :: Context -> [Variable] -> Map Variable (Set Object)
initVarCombi (_,objs) vars = Map.fromList $ zip vars (repeat $ Set.fromList objs)


-- | Takes an action, grounds it and then if the precondions are satisfied applies it to a state
apply' :: PDDLDomain -> PDDLProblem -> State -> Action -> Maybe State
apply' domain prob state action =
    do as <- actionSpec domain (aName action)
       if isSatisfied (contextFromAs (probObjs prob) as action) state (asPrecond as)
       then return $ applyEffects state $ instantiateAction (probObjs prob) as state action
       else Nothing

applyActionSpec :: ActionSpec -> [Name] -> Action
applyActionSpec aSpec args = (asName aSpec, args)

isActionSatisfied :: AllPossibleObjects
                  -> ActionSpec
                  -> Action
                  -> State
                  -> Bool
isActionSatisfied allobjs as act s =
  isSatisfied (contextFromAs allobjs as act) s (asPrecond as)

isPredSatisfied :: State -> Literal GroundedPredicate -> Bool
isPredSatisfied s (Pos p) = Set.member p s
isPredSatisfied s (Neg p) = not $ Set.member p s

isSatisfied :: Context -> State -> GoalDesc-> Bool
isSatisfied c s (GAnd l) = all (isSatisfied c s) l
isSatisfied c s (GOr l)  = any (isSatisfied c s) l
isSatisfied c s (GNot gd) = not $ isSatisfied c s gd
isSatisfied c s (GLit p) = isPredSatisfied s (fmap (ground c) p)


ground :: Context -> FluentPredicate -> GroundedPredicate
ground c (Predicate n args) = Predicate n $ map (instantiateTerm c) args

groundMany :: Context -> Set FluentPredicate -> Set GroundedPredicate
groundMany c = Set.map (ground c)

contextFromDom :: AllPossibleObjects -> PDDLDomain -> Action -> Context
contextFromDom objs domain act =  contextFromAs objs as act where
    as = findActionSpec domain act

contextFromAs :: AllPossibleObjects -> ActionSpec -> Action -> Context
contextFromAs objs as (_, args) = context objs (asParas as) args

emptyContext :: AllPossibleObjects -> Context
emptyContext allobjs = (Map.empty, allobjs)

context :: [Object] -> [Variable] -> [Object] -> Context
context allobjs para args = (Map.fromList $ zip para args, allobjs)
-- groundPreconditions :: ActionSpec -> [Name] -> GoalDesc
-- groundPreconditions as args = fmap (substitute (asParas as) args) (asPrecond as)

applicable :: PDDLProblem -> ActionSpec -> State -> [Object] -> Bool
applicable prob as s args =
  isSatisfied (context (probObjs prob) (asParas as) args) s (asPrecond as)


numberOfSatisfied :: Context -> State -> GoalDesc -> (Int,Int)
numberOfSatisfied c s (GAnd l) = sum *** sum $ unzip (map (numberOfSatisfied c s) l)
numberOfSatisfied _ _ (GOr [])  = (0,0)
numberOfSatisfied c s (GOr l)  = maximum $ map (numberOfSatisfied c s) l
numberOfSatisfied c s (GNot gd) = swap $ numberOfSatisfied c s gd
numberOfSatisfied c s gd@(GLit _) | isSatisfied c s gd = (1,0)
                                  | otherwise = (0,1)

numberOfPredicates :: GoalDesc -> Int
numberOfPredicates (GAnd l) = sum $ map numberOfPredicates l
numberOfPredicates (GOr [])  = 0
numberOfPredicates (GOr l)  = maximum $ map numberOfPredicates l
numberOfPredicates (GNot gd) = numberOfPredicates gd
numberOfPredicates (GLit _) = 1

applicableActions' :: PDDLProblem -> ActionSpec -> State -> [Action]
applicableActions' prob aSpec s = filter (isApplicable prob aSpec s  . aArgs) apps
    where update m (k, a) = Map.insertWith (++) a [k] m
          probTs = foldl update Map.empty $ Map.toList (probTypes prob)
          candidates = map (extractType . snd) (typeList aSpec)
          apps = map ((,) (asName aSpec)) (sequence candidates)
          extractType el =
            fromMaybe
              (error $ "applicableActions: attempted to look up "
                     ++ show el ++" in problem type map."
                     ++ show probTs ++ show (typeList aSpec))
              (Map.lookup el probTs)

instance ActionSpecification ActionSpec PDDLProblem where
    name           = asName
    arity          = length . asParas
    isApplicable   = applicable
    effect p as s args = instantiateAction (probObjs p) as s
                       $ applyActionSpec as args
    applicableActions = applicableActions'


instance Domain PDDLDomain PDDLProblem ActionSpec where
    actionSpecification = actionSpec
    actions             = dmActionsSpecs
    apply               = apply'
    allApplicableActions dom prob s =
        concatMap (flip (applicableActions prob) s) (actions dom)

instance Problem PDDLProblem where
    initialState = probState
    isSolved p   = (flip (isSatisfied (Map.empty,probObjs p)) . probGoal) p
    objects      = probObjs
    setInitialState prob s = prob { probState = s }

instance Graph PDDLGraph State Action where
  adjacentEdges (PDDLGraph (dom, prob)) = allApplicableActions dom prob
  edgeCost _ _ _ = 1
  adjacentVertex (PDDLGraph (dom, prob)) = apply' dom prob


instance ForwardSearchGraph PDDLGraph State Action where
  goalReached (PDDLGraph (_, prob)) = isSolved prob
  heuristicCostToGoal (PDDLGraph (_, prob)) s =
    numberOfPredicates (probGoal prob) -
    fst (numberOfSatisfied (emptyContext (probObjs prob)) s (probGoal prob))
