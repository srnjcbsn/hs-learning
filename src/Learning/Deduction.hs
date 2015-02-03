module Learning.Deduction where

import PDDL.Type
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Map (Map)

appendToAll :: [[a]] -> a -> [[a]]
appendToAll ll ele = List.map ((:) ele)  ll

listify :: [a] -> [[a]]
listify = List.map (:[])

mapMany :: Ord a => [a] -> [b] -> Map a [b]
mapMany a b = Map.fromAscListWith (++) $ List.zip a (listify b)

fluentVariants :: Map Object [Name] -> [Object] -> [ [Argument] ]
fluentVariants _ [] = [[]]
fluentVariants mapper (obj:rest) = allmapped
  where
    restMapped = fluentVariants mapper rest
    allmapped =
       case Map.lookup obj mapper of
        Just names -> List.concatMap (appendToAll restMapped . Ref) names
        Nothing -> appendToAll restMapped $ Const obj

deduct :: Parameters-> Arguments -> GroundedPredicate -> [FluentPredicate]
deduct paras args (name, predObjs) = collect
  where
    objsMap = mapMany args paras
    toFluent objs = (name, objs)
    variants = fluentVariants objsMap predObjs
    collect = List.map toFluent variants

deductFromAction :: ActionSpec -> Action -> GroundedPredicate -> [FluentPredicate]
deductFromAction as (_, args) = deduct (asParas as) args
