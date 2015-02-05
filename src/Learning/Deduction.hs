module Learning.Deduction(expandFluents, deduct, collectDeducts, collectManyDeduct, combineDeductions, variants, asPDDL, unground) where

import qualified Data.List as List
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Set  (Set)
import qualified Data.Set  as Set
import           PDDL.Type

-- | returns an unambiguous predicate, if any
unambiguate :: Set FluentPredicate -- ^ possible predicates
            -> Set FluentPredicate -- ^ ungrounded predicates
            -> Maybe FluentPredicate -- ^ an unambiguous predicate, if it can be found
unambiguate = undefined

reducePossibilities :: Set FluentPredicate
                    -> [Set FluentPredicate]
                    -> Set FluentPredicate
reducePossibilities = undefined

appendToAll :: [[a]] -> a -> [[a]]
appendToAll ll ele = List.map ((:) ele)  ll

listify :: [a] -> [[a]]
listify = List.map (:[])


mapMany :: Ord a => [a] -> [b] -> Map a [b]
mapMany a b = Map.fromAscListWith (++) $ List.zip a (listify b)

eitherLookup :: (Ord a, Ord b) => Map a [b] -> a -> Set (Either a b)
eitherLookup mapAB a =
  case Map.lookup a mapAB of
    Just b -> Set.fromList $ List.map Right b
    Nothing -> Set.singleton $ Left a

deduct :: (Ord arg, Ord para) => [para] -> [arg] -> [arg] -> [Set (Either arg para)]
deduct paras args res =  deduction
  where
    objsMap = mapMany args paras
    deduction = List.map (eitherLookup objsMap) res

collectDeducts :: Ord a => [ Set a ] -> [Set a] -> [Set a]
collectDeducts d1 d2 = combined
  where
    combineLists (l1,l2) = Set.intersection l1 l2
    zipped = List.zip d1 d2
    combined = List.map combineLists zipped

collectManyDeduct :: Ord a => [ [ Set a ] ] -> [ Set a  ]
collectManyDeduct = List.foldl1 collectDeducts

combine :: Ord a => [Set a] -> [Set a] -> [Set a] -> [[Set a]]
combine _ _ [] = []
combine fl1 (h1:rest1) (h2:rest2) = (fl1 ++ [Set.union h1 h2] ++ rest1) : combine (fl1 ++ [h1]) rest1 rest2

merge :: Ord a => Set [Set a] -> [Set a] -> Set [Set a]
merge full single = mapped
  where
    combiner a = Set.fromList $ combine [] single a
    mapped = Set.foldl Set.union Set.empty $ Set.map combiner full

combineDeductions :: Ord a => [ [ Set a ] ] -> [ [ Set a ] ] -> [ [ Set a ] ]
combineDeductions old new  = asList
  where
    setFunc l = List.foldl1 Set.union ( List.map (Set.fromList . variants) l )
    oldSet = setFunc old
    newSet = setFunc new
    difs = Set.intersection oldSet newSet
    (h:newForm) = Set.toList $ Set.map (List.map Set.singleton) difs
    folded = List.foldl merge (Set.singleton h) newForm
    asList = Set.toList folded



variants :: [ Set a ] -> [ [a] ]
variants [] = [ [] ]
variants (args:rest) = allmapped
  where
    restMapped = variants rest
    allmapped = List.concatMap (appendToAll restMapped) (Set.toList args)

asPDDL :: [Set (Either Object Name)] -> [Set Argument]
asPDDL res = pddl
  where
    toPDDL v =
      case v of
       Left arg -> Const arg
       Right p -> Ref p
    pddl = List.map (Set.map toPDDL) res

unground :: Parameters -> Arguments -> [Object] -> [Set Argument]
unground paras args objs = deduction
  where
    deduction = asPDDL $ deduct paras args objs

expandFluents :: [Set Argument] -> Name -> [FluentPredicate]
expandFluents argOptions name = preds
  where
    predArgs = variants argOptions
    preds = List.map (\x -> (name, x)) predArgs
