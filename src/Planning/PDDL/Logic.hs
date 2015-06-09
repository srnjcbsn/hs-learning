module Planning.PDDL.Logic
    ( apply
    , findActionSpec
    , instantiateAction
    , ground
    , applyAction
    , isSatisfied
    , contextFromDom
    , contextFromAs
    , context
    , groundMany
    ) where

import           Data.List     (intercalate)
-- import qualified Data.List     as List
import           Control.Arrow ((***))
import           Control.Monad (replicateM)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe    (fromMaybe)
import           Data.Set      (Set, (\\))
import qualified Data.Set      as Set
import           Data.Tuple
import qualified Data.TupleSet as TSet
-- import           Data.Tuple    (swap)
-- import qualified Data.TupleSet as TSet
import           Graph
import           Graph.Search
import           Logic.Formula
import           Planning
import           Planning.PDDL
-- import           Data.Maybe

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


-- -- | Instantiates a formula into the actual positive and negative changes
-- insForm :: Map Argument Object -> Formula Argument -> GroundedChanges
-- insForm m (Pred p) =
--     (Set.singleton (Predicate (pName p) (mapMaybe (`Map.lookup` m) $ pArgs p)), Set.empty)
-- insForm m (Neg f) = swap $ insForm m f
-- insForm m (Con fs) =
--     List.foldl (\changes f -> TSet.union changes $ insForm m f ) TSet.empty fs

-- | Checks if the preconditions of a grounded action are satisfied
-- isActionValid :: GroundedAction -> Bool
-- isActionValid = fst

-- | Applies the grounded actions to a state, if the action is not valid nothing is returned
applyAction :: State -> GroundedEffects -> State
applyAction s (posEff,negEff) = Set.union (Set.difference s negEff) posEff


-- | Instantiates a formula into the actual positive and negative changes
instantiateAction :: AllPossibleObjects -> ActionSpec -> State -> Action ->  GroundedEffects
instantiateAction espec as s a =
  instantiateEffects (contextFromAs espec as a) s (asEffect as)

instantiateEffects :: Context -> State -> Effect -> GroundedEffects
instantiateEffects c s (EAnd l) = TSet.unions $ map (instantiateEffects c s ) l
instantiateEffects c s (EForall vars (EWhen gd e)) = undefined
instantiateEffects c s (EForall vars l) = undefined
instantiateEffects c s (EWhen gd e)
    | isSatisfied c s gd = instantiateEffects c s e
    | otherwise = TSet.empty
instantiateEffects c _ (ELit (Neg p)) = (Set.empty, Set.singleton $ ground c p)
instantiateEffects c _ (ELit (Pos p)) = (Set.singleton $ ground c p, Set.empty)

satisfyingContexts :: Context
                   -> [Variable]
                   -> State
                   -> GoalDesc
                   -> [Context]
satisfyingContexts c@(_,allobjs) vars s gd = undefined where
  combis = satisfyingCombi c vars s gd


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

    -- p(x,y) and p(x,z)
    -- objs = [1,2,3,4]

    --        n(1,2) n(1,3) n(1,4)
    --        n(2,2)        n(2,4)
    -- n(3,1) n(3,2)        n(3,4)
    -- n(4,1) n(4,2) n(4,3) n(4,4)

    -- p(1,1)
    -- p(2,1)        p(2,3)
    --               p(3,3)

    -- p(x,y) - x = (1,2,3) y = (1,3)   z = (1..4)

    -- p(z,x) - x = (1,3)   y = (1..4)  z = (1,2,3)

    -- and    - x = (1,3)   y = (1,3)   z = (1,2,3)




    -- p(x,y) = x=1 y=1  p(1,1)
    --        = x=2 y=1  p(2,1)
    --        = x=3 y=1  p(3,1) X
    --        = x=1 y=3  p(1,3) X
    --        = x=2 y=3  p(2,3)
    --        = x=3 y=3  p(3,3)
    --        = x=4 y=_  p(4,_) X
    -- p(z,x) = x=2 z=_  p(_,2)
    --np(z,x)

    -- m = constMap (asConstants as)
    -- pairs = List.zip (List.map Ref $ asParas as) (aArgs a)
    -- paraMap = Map.fromList pairs
    -- fullMap = Map.union paraMap m
    -- ga = (groundPreconditions as (aArgs a), insForm fullMap (asEffect as))


-- substMap :: [Name] -> [Name] -> Map Name Name
-- substMap paras args = Map.fromList $ zip paras args
--
-- subst :: Map Name Name -> Term -> Name
-- subst m (TVar r)   = m ! r
-- subst _ (TName c) = c
--
-- substitute :: [Name] -> [Name] -> Term -> Name
-- substitute paras args = subst $ substMap paras args

-- ground' :: [Name] -> [Name] -> Set FluentPredicate -> Set GroundedPredicate
-- ground' paras args = Set.map (fmap $ substitute paras args)

-- ground :: PDDLDomain
--        -> Action
--        -> Set FluentPredicate
--        -> Set GroundedPredicate
-- ground domain (n, args) = groundPred (asParas as) args where
--     as = findActionSpec domain (n, args)

-- | Takes an action, grounds it and then if the precondions are satisfied applies it to a state
apply' :: PDDLDomain -> PDDLProblem -> State -> Action -> Maybe State
apply' domain prob state action =
    do as <- actionSpec domain (aName action)
       if isSatisfied (contextFromAs (probObjs prob) as action) state (asPrecond as)
       then return $ applyAction state $ instantiateAction (probObjs prob) as state action
       else Nothing

applyActionSpec :: ActionSpec -> [Name] -> Action
applyActionSpec aSpec args = (asName aSpec, args)

isPredSatisfied :: Literal GroundedPredicate -> State -> Bool
isPredSatisfied (Pos p) s = Set.member p s
isPredSatisfied (Neg p) s = not $ Set.member p s

isSatisfied :: Context -> State -> GoalDesc-> Bool
isSatisfied c s (GAnd l) = all (isSatisfied c s) l
isSatisfied c s (GOr l)  = any (isSatisfied c s) l
isSatisfied c s (GNot gd) = not $ isSatisfied c s gd
isSatisfied c s (GLit p) = isPredSatisfied (fmap (ground c) p) s


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

context :: [Object] -> [Name] -> [Object] -> Context
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
  heuristicCostToGoal (PDDLGraph (dom, prob)) s =
    numberOfPredicates (probGoal prob) -
    fst (numberOfSatisfied (emptyContext (probObjs prob)) s (probGoal prob))
