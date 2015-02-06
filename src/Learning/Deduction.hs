module Learning.Deduction where

import qualified Data.List as List
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Set  (Set)
import qualified Data.Set  as Set
import           PDDL.Type

-- | Returns an unambiguous predicate, if any
unambiguate :: Set FluentPredicate   -- ^ possible predicates
            -> Set FluentPredicate   -- ^ ungrounded predicates
            -> Maybe FluentPredicate -- ^ an unambiguous predicate, if it can be found
unambiguate unknowns ungrounds = mUnamp
  where
    comb = Set.intersection unknowns ungrounds
    mUnamp | Set.size comb == 1 = Just (List.head $ Set.toList comb)
           | otherwise = Nothing

-- | Reduces possibilities of ungrounded predicates
reducePossibilities :: Set FluentPredicate    -- ^ the current set of all possibilities
                    -> [Set FluentPredicate]  -- ^ all sets of ungrounded predicates
                    -> Set FluentPredicate    -- ^ the new set of all possibilities
reducePossibilities unknown others = Set.intersection unknown $ List.foldl1 Set.union others

-- | Append an element to all lists in the list
appendToAll :: [[a]] -> a -> [[a]]
appendToAll ll ele = List.map ((:) ele)  ll

-- | Turns each element of the list into a list of one
listify :: [a] -> [[a]]
listify = List.map (:[])

-- | Maps list a to list b,
--   if two elements in the a list is equal then it appends the two b's together
mapMany :: Ord a => [a] -> [b] -> Map a [b]
mapMany a b = Map.fromListWith (++) $ List.zip a (listify b)

-- | Looks up a in the map,
--   if a is not found then it returns a. the elements are returns as a Set
eitherLookup :: (Ord a, Ord b) => Map a [b] -> a -> Set (Either a b)
eitherLookup mapAB a =
  case Map.lookup a mapAB of
    Just b -> Set.fromList $ List.map Right b
    Nothing -> Set.singleton $ Left a

-- | Deducts all combinations of arguments which could be used in a function
--   Returns a Set for each pos [Arg1, Arg2... ArgN] that contains the possible paramters,
--   if the argument used was not part of arguments passed then that argument(as Left) will be used
--   F(x,y) called with [1,2] which produces [2,1] will make the deduct return [{y}, {x}] as x = 2 and y = 1
deduct :: (Ord arg, Ord para) => [para] -> [arg] -> [arg] -> [Set (Either arg para)]
deduct paras args res =  deduction
  where
    objsMap = mapMany args paras
    deduction = List.map (eitherLookup objsMap) res

-- | Provides all variants of argument combinations
--   given [{x,y},{z,v}] variants will produce [x,z], [x,v], [y,z] [y,v]
variants :: [ Set a ] -> [ [a] ]
variants [] = [ [] ]
variants (args:rest) = allmapped
  where
    restMapped = variants rest
    allmapped = List.concatMap (appendToAll restMapped) (Set.toList args)

-- | Turns the output produce from Deducts into PDDL format
--   it will assume objects are Constants
asPDDL :: [Set (Either Object Name)] -> [Set Argument]
asPDDL res = pddl
  where
    toPDDL v =
      case v of
       Left arg -> Const arg
       Right p -> Ref p
    pddl = List.map (Set.map toPDDL) res

-- | Takes a grounded predicate and produces all ungrounded possibilities
unground :: Parameters -- ^ the paramters of the action spec
          -> Arguments -- ^ the arguments the action was executed with
          -> [Object]  -- ^ the predicate it produced
          -> [Set Argument] -- ^ a list of possibilities [Arg1, Arg2,..., ArgN ] where the Args are sets of options
unground paras args objs = deduction
  where
    deduction = asPDDL $ deduct paras args objs

-- | Expands the output of unground and provides all the ungrounded predicates that could be produced
--   given [{x,y}, {x}] and "p" produces p(x,x) p(y,x)
expandFluents :: [Set Argument] -> Name -> [FluentPredicate]
expandFluents argOptions name = preds
  where
    predArgs = variants argOptions
    preds = List.map (\x -> (name, x)) predArgs
