module Planning.PDDL.Logic
    ( isActionValid
    , apply
    , findActionSpec
    , instantiateAction
    , ground
    , applyAction
    , isSatisfied
    ) where

import           Data.List     (intercalate)
import qualified Data.List     as List
import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Tuple    (swap)
import qualified Data.TupleSet as TSet
import           Graph
import           Graph.Search
import           Logic.Formula
import           Planning
import           Planning.PDDL
import           Data.Maybe

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
isActionValid :: GroundedAction -> Bool
isActionValid =  fst

-- | Applies the grounded actions to a state, if the action is not valid nothing is returned
applyAction :: State -> GroundedAction -> Maybe State
applyAction s act@(_,(posEff,negEff))
    | isActionValid act = Just $ Set.union (Set.difference s negEff) posEff
    | otherwise           = Nothing

constMap :: [Name] -> Map Term Object
constMap consts =
    Map.fromList $ List.map (\n -> (TName n, n)) consts

-- | Instantiates a formula into the actual positive and negative changes
instantiateAction :: ActionSpec -> Action -> GroundedAction
instantiateAction as a = undefined where
    -- m = constMap (asConstants as)
    -- pairs = List.zip (List.map Ref $ asParas as) (aArgs a)
    -- paraMap = Map.fromList pairs
    -- fullMap = Map.union paraMap m
    -- ga = (groundPreconditions as (aArgs a), insForm fullMap (asEffect as))


substMap :: [Name] -> [Name] -> Map Name Name
substMap paras args = Map.fromList $ zip paras args

subst :: Map Name Name -> Term -> Name
subst m (TVar r)   = m ! r
subst _ (TName c) = c

substitute :: [Name] -> [Name] -> Term -> Name
substitute paras args = subst $ substMap paras args

ground' :: [Name] -> [Name] -> Set FluentPredicate -> Set GroundedPredicate
ground' paras args = Set.map (fmap $ substitute paras args)

ground :: PDDLDomain
       -> Action
       -> Set FluentPredicate
       -> Set GroundedPredicate
ground domain (n, args) = ground' (asParas as) args where
    as = findActionSpec domain (n, args)

-- | Takes an action, grounds it and then if the precondions are satisfied applies it to a state
apply' :: PDDLDomain -> State -> Action -> Maybe State
apply' domain state action =
    case actionSpec domain (aName action) of
        Just aSpec -> applyAction state $ instantiateAction aSpec action
        Nothing    -> Nothing

applyActionSpec :: ActionSpec -> [Name] -> Action
applyActionSpec aSpec args = (asName aSpec, args)

isSatisfied :: GoalDesc -> State -> Bool
isSatisfied = undefined

-- groundPreconditions :: ActionSpec -> [Name] -> GoalDesc
-- groundPreconditions as args = fmap (substitute (asParas as) args) (asPrecond as)

applicable :: ActionSpec -> State -> [Name] -> Bool
applicable as s args = undefined

numberOfSatisfied :: GoalDesc -> State -> Int
numberOfSatisfied _ _ = 1
-- numberOfSatisfied (Pred p) s | Set.member p s = 1
--                              | otherwise = 0
--
-- numberOfSatisfied (Neg f) s | not $ isSatisfied f s = numberOfSatisfied f s
--                             | otherwise = 0
-- numberOfSatisfied (Con fs) s = sum $ map (`numberOfSatisfied` s) fs

numberOfPredicates :: GoalDesc -> Int
numberOfPredicates _ = 1
-- numberOfPredicates (Pred _) = 1
-- numberOfPredicates (Neg f) = numberOfPredicates f
-- numberOfPredicates (Con fs) = sum $ map numberOfPredicates fs

applicableActions' :: PDDLProblem -> State -> ActionSpec -> [Action]
applicableActions' prob s aSpec = filter (isApplicable aSpec s . aArgs) apps
    where update m (k, a) = Map.insertWith (++) a [k] m
          probTs = foldl update Map.empty $ Map.toList (probTypes prob)
          candidates = map (extractType . snd) (typeList aSpec)
          apps = map ((,) (asName aSpec)) (sequence candidates)
          extractType el =
              case Map.lookup el probTs of
                   Just ls -> ls
                   Nothing -> error $ "applicableActions: attempted to look up "
                                      ++ show el ++ " in problem type map."
                                      ++ show probTs ++ show (typeList aSpec)


instance ActionSpecification ActionSpec PDDLProblem where
    name           = asName
    arity          = length . asParas
    isApplicable   = applicable
    effect as args = snd $ instantiateAction as $ applyActionSpec as args
    applicableActions = applicableActions'


instance Domain PDDLDomain PDDLProblem ActionSpec where
    actionSpecification = actionSpec
    actions             = dmActionsSpecs
    apply               = apply'
    allApplicableActions dom prob s =
        concatMap (applicableActions prob s) (actions dom)

instance Problem PDDLProblem where
    initialState = probState
    isSolved     = isSatisfied . probGoal
    objects      = probObjs
    setInitialState prob s = prob { probState = s }

instance Graph PDDLGraph State Action where
  adjacentEdges (PDDLGraph (dom, prob)) s = allApplicableActions dom prob s
  edgeCost _ _ _ = 1
  adjacentVertex (PDDLGraph (dom, _)) s act = apply' dom s act


instance ForwardSearchGraph PDDLGraph State Action where
  goalReached (PDDLGraph (_, prob)) s = isSolved prob s
  heuristicCostToGoal (PDDLGraph (_, prob)) s =
    (numberOfPredicates (probGoal prob)) - (numberOfSatisfied (probGoal prob) s)
