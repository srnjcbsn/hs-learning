module PDDL.Logic ( isActionValid
                  , apply
                  , findActionSpec
                  , instantiateFormula
                  ) where

import PDDL.Type
import qualified Data.List as List
import           Data.Map  (Map)
import qualified Data.Map as Map
import           Data.Set  (Set)
import qualified Data.Set as Set
import           Data.Tuple (swap)



findActionSpec :: Domain -> Action -> Maybe ActionSpec
findActionSpec domain action = actionsSpec
  where
    specs = dmActionsSpecs domain
    actname = aName action
    actionsSpec = List.find (\as -> asName as == actname) specs

unionTuple :: Ord a => (Set a, Set a) -> (Set a, Set a) -> (Set a, Set a)
unionTuple (pos,neg) (pos2,neg2) = (Set.union pos pos2, Set.union neg neg2)

instantiateFormula :: Map Argument Object -> Formula -> GroundedChanges
instantiateFormula m (Predicate p) = (Set.singleton (pName p, List.map (m Map.!) $ pArgs p), Set.empty)
instantiateFormula m (Neg f) = swap $ instantiateFormula m f
instantiateFormula m (Con fs) = List.foldl (\changes f -> unionTuple changes $ instantiateFormula m f ) (Set.empty,Set.empty) fs

instantiateAction :: Map Argument Object -> ActionSpec -> Action -> GroundedAction
instantiateAction  m as act = ga
  where
    pairs = List.zip (List.map Ref $ asParas as) (aArgs act)
    paraMap = Map.fromAscList pairs
    fullMap = Map.union paraMap m
    ga = (instantiateFormula fullMap (asPrecond as), instantiateFormula fullMap (asEffect as))

isActionValid :: State -> GroundedAction -> Bool
isActionValid s ((posCond,negCond),(posEff,negEff)) =
  Set.isSubsetOf posCond s &&
  Set.null (Set.intersection negCond s)

applyAction :: State -> GroundedAction -> Maybe State
applyAction s act@(_,(posEff,negEff)) =
    if isActionValid s act then
      Just $ Set.union (Set.difference s negEff) posEff
    else Nothing


apply :: Domain -> State -> Action -> Maybe State
apply domain curState action = newState
  where
    mapDomain = Map.fromAscList $ List.map (\n -> (Const n, n)) (dmConstants domain)
    newState = case findActionSpec domain action of
                Just actSpec -> out
                  where
                    gact@(_,(pos,neg)) = instantiateAction mapDomain actSpec action
                    ambiguousEffects = Set.intersection pos neg
                    out = if Set.null ambiguousEffects
                          then applyAction curState gact
                          else error $ "ambiguous effects in " ++ aName action ++ ": " ++ show (Set.toList ambiguousEffects)

                Nothing      -> Nothing
