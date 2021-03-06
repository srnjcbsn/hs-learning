module Learning.Induction where

import qualified Data.List as List
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Planning.PDDL
import           Control.Monad      (replicateM)
import Logic.Formula

-- | Returns an unambiguous predicate, if any
unambiguate :: Set FluentPredicate   -- ^ possible predicates
            -> Set FluentPredicate   -- ^ ungrounded predicates
            -> Either FluentPredicate (Set FluentPredicate) -- ^ an unambiguous predicate, if it can be found
unambiguate unknowns ungrounds = mUnamp
  where
    comb = Set.intersection unknowns ungrounds
    mUnamp | Set.size comb == 1 = Left (List.head $ Set.toList comb)
           | otherwise = Right comb

-- | Reduces possibilities of ungrounded predicates
reducePossibilities :: Set FluentPredicate    -- ^ the current set of all possibilities
                    -> [Set FluentPredicate]  -- ^ all sets of ungrounded predicates to remove
                    -> Set FluentPredicate    -- ^ the new set of all possibilities
reducePossibilities unknown others = unknown `Set.difference` Set.unions others

-- | Append an element to all lists in the list
appendToAll :: [[a]] -> a -> [[a]]
appendToAll ll ele = List.map ((:) ele)  ll

-- | Turns each element of the list into a list of one
listify :: [a] -> [[a]]
listify = List.map (:[])

-- | Maps list a to list b,
--   if two elements in the a list is equal then it appends the two b's together.
mapMany :: Ord a => [a] -> [b] -> Map a [b]
mapMany a b = Map.fromListWith (++) $ List.zip a (listify b)

-- | Looks up a in the map,
--   if a is not found then it returns a. the elements are returns as a Set
eitherLookup :: (Ord a, Ord b) => Map a [b] -> a -> Set (Either a b)
eitherLookup mapAB a =
  case Map.lookup a mapAB of
    Just b -> Set.fromList $ List.map Right b
    Nothing -> Set.singleton $ Left a


-- | Inducts all combinations of arguments which could be used in a function.
--   Returns a Set as arg for each position IE. [Arg1, Arg2... ArgN] that contains the possible paramters,
--   if the argument used was not part of arguments passed then that argument(as Left) will be used.
--   f(x,y) was called with f(1,2) which produced p(2,1)
--   in that case deducts returns [{y}, {x}] since its obvious that x = 2 and y = 1
induct :: (Ord arg, Ord para) => [para] -> [arg] -> [arg] -> [Set (Either arg para)]
induct paras args res =  induction
  where
    objsMap = mapMany args paras
    induction = List.map (eitherLookup objsMap) res

-- | Provides all variants of argument combinations
--   given [{x,y},{z,v}] variants will produce [x,z], [x,v], [y,z] [y,v]
variants :: [ Set a ] -> [ [a] ]
variants [] = [ [] ]
variants (args:rest) = allmapped
  where
    restMapped = variants rest
    allmapped = List.concatMap (appendToAll restMapped) (Set.toList args)

allFluents :: [Term] -> PredicateSpec -> Set FluentPredicate
allFluents paras (Predicate name args) =
        Set.fromList $ map (Predicate name) $ replicateM (length args) paras

-- | Turns the output produce from Deducts into PDDL format
--   it will assume objects are Constants
asPDDL :: [Set (Either Object Name)] -> [Set Term]
asPDDL res = pddl
  where
    toPDDL v =
      case v of
       Left arg -> TName arg
       Right p -> TVar p
    pddl = List.map (Set.map toPDDL) res

-- | Takes a grounded predicate and produces all ungrounded possibilities
ungroundCollected ::  [Name] -- ^ the paramters of the action spec
          -> [Object] -- ^ the arguments the action was executed with
          -> [Object]  -- ^ the predicate it produced
          -> [Set Term] -- ^ a list of possibilities [Arg1, Arg2,..., ArgN ] where the Args are sets of options
ungroundCollected paras args objs = induction
  where
    induction = asPDDL $ induct paras args objs

unground :: [Name] -> [Object] -> GroundedPredicate -> Set FluentPredicate
unground paras args gp = Set.fromList
                              $ expandFluents (predName gp)
                              $ ungroundCollected paras args (predArgs gp)

-- | Expands the output of unground and provides all the ungrounded predicates that could be produced
--   given [{x,y}, {x}] and "p" produces p(x,x) p(y,x)
expandFluents :: Name -> [Set Term] -> [FluentPredicate]
expandFluents name argOptions = preds
  where
    args = variants argOptions
    preds = List.map (Predicate name) args
