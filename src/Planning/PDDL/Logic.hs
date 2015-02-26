module Planning.PDDL.Logic
    ( isActionValid
    , apply
    , findActionSpec
    , instantiateFormula
    , instantiateAction
    , ground
    , applyAction
    ) where
import           Data.List     (intercalate)
import qualified Data.List     as List
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Tuple    (swap)
import           Planning
import           Planning.PDDL
import           Graph.Search
import           Graph
import qualified Data.TupleSet as Set2
import Logic.Formula


-- | Finds the action spec of an action in a domain
findActionSpec :: PDDLDomain -> Action -> ActionSpec
findActionSpec domain (name, _) =
    case actionSpec domain name of
        Just aSpec -> aSpec
        Nothing    ->
            error $  "Action specification with name " ++ name
                  ++ " does not exist in domain (action names: "
                  ++ names ++ ")"
            where names = intercalate ","
                        $ map asName (dmActionsSpecs domain)


-- | Instantiates a formula into the actual positive and negative changes
insForm :: Map Argument Object -> Formula Argument -> GroundedChanges
insForm m (Pred p) =
    (Set.singleton (Predicate (pName p) (List.map (m Map.!) $ pArgs p)), Set.empty)
insForm m (Neg f) = swap $ insForm m f
insForm m (Con fs) = List.foldl (\changes f -> Set2.union changes $ insForm m f ) (Set.empty,Set.empty) fs

-- | instantiates an Action into the actual precondions and the actual effect
insAct :: Map Argument Object -> ActionSpec -> Action -> GroundedAction
insAct m as act = ga
  where
    pairs = List.zip (List.map Ref $ asParas as) (aArgs act)
    paraMap = Map.fromList pairs
    fullMap = Map.union paraMap m
    ga = (insForm fullMap (asPrecond as), insForm fullMap (asEffect as))

-- | Checks if the preconditions of a grounded action are satisfied
isActionValid :: State -> GroundedAction -> Bool
isActionValid s ((posCond, negCond), _) =
  Set.isSubsetOf posCond s &&
  Set.null (Set.intersection negCond s)

-- | Applies the grounded actions to a state, if the action is not valid nothing is returned
applyAction :: State -> GroundedAction -> Maybe State
applyAction s act@(_,(posEff,negEff)) =
    if isActionValid s act then
      Just $ Set.union (Set.difference s negEff) posEff
    else Nothing

domainMap :: PDDLDomain -> Map Argument Object
domainMap domain = constMap (dmConstants domain)

constMap :: [Name] -> Map Argument Object
constMap consts =
    Map.fromList $ List.map (\n -> (Const n, n)) consts

-- | Instantiates a formula into the actual positive and negative changes
instantiateFormula :: PDDLDomain
                   -> [Name]          -- ^ Parameters
                   -> [Name]          -- ^ Arguments
                   -> Formula Argument
                   -> GroundedChanges
instantiateFormula domain paras args form = insForm fullMap form
    where pairs = List.zip (List.map Ref paras) args
          paraMap = Map.fromList pairs
          fullMap = Map.union paraMap $ domainMap domain


-- | Instantiates a formula into the actual positive and negative changes
instantiateAction :: ActionSpec -> Action -> GroundedAction
instantiateAction as a = ga where
    m = constMap (asConstants as)
    pairs = List.zip (List.map Ref $ asParas as) (aArgs a)
    paraMap = Map.fromList pairs
    fullMap = Map.union paraMap m
    ga = (insForm fullMap (asPrecond as), insForm fullMap (asEffect as))

ground :: PDDLDomain
       -> Action
       -> Set FluentPredicate
       -> Set GroundedPredicate
ground domain (name, args) fluents =
    fst $ instantiateFormula domain (asParas aSpec) args
        (Con $ List.map Pred $ Set.toList fluents)
        where aSpec = findActionSpec domain (name, args)

-- | Takes an action, grounds it and then if the precondions are satisfied applies it to a state
apply' :: PDDLDomain -> State -> Action -> Maybe State
apply' domain state action =
    case actionSpec domain (aName action) of
        Just aSpec -> applyAction state $ instantiateAction aSpec action
        Nothing    -> Nothing


applyActionSpec :: ActionSpec -> [Name] -> Action
applyActionSpec aSpec args = (asName aSpec, args)

isSatisfied :: Formula Name -> State -> Bool
isSatisfied (Pred p) s = Set.member p s
isSatisfied (Neg f)  s = not $ isSatisfied f s
isSatisfied (Con fs) s = all (`isSatisfied` s) fs

numberOfSatisfied :: Formula Name -> State -> Int
numberOfSatisfied (Pred p) s | Set.member p s = 1
                             | otherwise = 0

numberOfSatisfied (Neg f) s | not $ isSatisfied f s = numberOfSatisfied f s
                            | otherwise = 0
numberOfSatisfied (Con fs) s = foldl (+) 0 $ map (`numberOfSatisfied` s) fs

numberOfPredicates :: Formula Name -> Int
numberOfPredicates (Pred _) = 1
numberOfPredicates (Neg f) = numberOfPredicates f
numberOfPredicates (Con fs) = foldl (+) 0 $ map (numberOfPredicates) fs

instance ActionSpecification ActionSpec where
    name         = asName
    arity        = length . asParas
    isApplicable as s args =
        isActionValid s $ instantiateAction as $ applyActionSpec as args
    effect as args = snd $ instantiateAction as $ applyActionSpec as args


instance Domain PDDLDomain ActionSpec where
    actionSpecification = actionSpec
    actions             = dmActionsSpecs
    apply               = apply'

instance Problem PDDLProblem where
    initialState = probState
    isSolved     = isSatisfied . probGoal
    objects      = probObjs

instance Graph PDDLGraph State Action where
  adjacentEdges (PDDLGraph (dom, prob)) s =
    let acts = actions dom
     in concatMap (applicableActions prob s) acts
  edgeCost _ _ _ = 1
  adjacentVertex (PDDLGraph (dom, _)) s act = apply' dom s act


instance ForwardSearchGraph PDDLGraph State Action where
  goalReached (PDDLGraph (_, prob)) s = isSolved prob s
  heuristicCostToGoal (PDDLGraph (_, prob)) s =
    (numberOfPredicates (probGoal prob)) - (numberOfSatisfied (probGoal prob) s)
