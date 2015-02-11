module PDDL.Logic (isActionValid
                  , apply
                  , findActionSpec
                  , instantiateFormula
                  , instantiateAction
                  , ground
                  ) where

import PDDL.Type
import qualified Data.List as List
import           Data.Map  (Map)
import qualified Data.Map as Map
import           Data.Set  (Set)
import qualified Data.Set as Set
import           Data.Tuple (swap)



-- | Finds the action spec of an action in a domain
findActionSpec :: Domain -> Action -> Maybe ActionSpec
findActionSpec domain action = actionsSpec
  where
    specs = dmActionsSpecs domain
    actname = aName action
    actionsSpec = List.find (\as -> asName as == actname) specs

-- | Gets two unions of a tuples with sets
unionTuple :: Ord a => (Set a, Set a) -> (Set a, Set a) -> (Set a, Set a)
unionTuple (pos,neg) (pos2,neg2) = (Set.union pos pos2, Set.union neg neg2)

-- | Instantiates a formula into the actual positive and negative changes
insForm :: Map Argument Object -> Formula -> GroundedChanges
insForm m (Predicate p) = (Set.singleton (pName p, List.map (m Map.!) $ pArgs p), Set.empty)
insForm m (Neg f) = swap $ insForm m f
insForm m (Con fs) = List.foldl (\changes f -> unionTuple changes $ insForm m f ) (Set.empty,Set.empty) fs

-- | instantiates an Action into the actual precondions and the actual effect
insAct :: Map Argument Object -> ActionSpec -> Action -> GroundedAction
insAct m as act = ga
  where
    pairs = List.zip (List.map Ref $ asParas as) (aArgs act)
    paraMap = Map.fromAscList pairs
    fullMap = Map.union paraMap m
    ga = (insForm fullMap (asPrecond as), insForm fullMap (asEffect as))

-- | Checks if the preconditions of a grounded action are satisfied
isActionValid :: State -> GroundedAction -> Bool
isActionValid s ((posCond,negCond),(posEff,negEff)) =
  Set.isSubsetOf posCond s &&
  Set.null (Set.intersection negCond s)

-- | Applies the grounded actions to a state, if the action is not valid nothing is returned
applyAction :: State -> GroundedAction -> Maybe State
applyAction s act@(_,(posEff,negEff)) =
    if isActionValid s act then
      Just $ Set.union (Set.difference s negEff) posEff
    else Nothing

domainMap :: Domain -> Map Argument Object
domainMap domain = Map.fromList $ List.map (\n -> (Const n, n)) (dmConstants domain)

-- | Instantiates a formula into the actual positive and negative changes
instantiateFormula :: Domain -> Formula -> GroundedChanges
instantiateFormula domain form =
  let mapDomain = domainMap domain in
    insForm mapDomain form

-- | Instantiates a formula into the actual positive and negative changes
instantiateAction :: Domain -> ActionSpec -> Action  -> GroundedAction
instantiateAction domain as a =
  let mapDomain = domainMap domain in
    insAct mapDomain as a

ground :: Set FluentPredicate -> Set GroundedPredicate
ground = undefined

-- | Takes an action, grounds it and then if the precondions are satisfied applies it to a state
--   If there are ambiguous effect an error is thrown
apply :: Domain -> State -> Action -> Maybe State
apply domain curState action = newState
  where
    mapDomain = domainMap domain
    newState = case findActionSpec domain action of
                Just actSpec -> out
                  where
                    gact@(_,(pos,neg)) = insAct mapDomain actSpec action
                    ambiguousEffects = Set.intersection pos neg
                    out = if Set.null ambiguousEffects
                          then applyAction curState gact
                          else error $ "ambiguous effects in " ++ aName action ++ ": " ++ show (Set.toList ambiguousEffects)

                Nothing      -> Nothing
