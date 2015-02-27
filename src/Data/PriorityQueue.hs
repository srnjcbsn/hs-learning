module Data.PriorityQueue
  ( insert
  , singleton
  , dequeueMin
  , fromList
  , empty
  , isEmpty
  , priority
  , PriorityQueue) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

newtype PriorityQueue a = PriorityQueue (Set (Int,a), Map a Int)
  deriving (Eq, Read, Show, Ord)


insert  :: Ord a => Int -> a -> PriorityQueue a -> PriorityQueue a
insert prio v (PriorityQueue (s, m)) =
  let mOldPrio = Map.lookup v m
      add s' = PriorityQueue (Set.insert (prio,v) s', Map.insert v prio m)
   in case mOldPrio of
      Just oldPrio -> add (Set.delete (oldPrio, v) s)
      Nothing -> add s

dequeueMin  :: Ord a => PriorityQueue a -> Maybe (a,  PriorityQueue a)
dequeueMin (PriorityQueue (s, m)) =
  do ((_, v), s') <- Set.minView s
     return (v, PriorityQueue (s', Map.delete v m))

fromList :: Ord a => [(Int, a)] -> PriorityQueue a
fromList =
  let insert' q (p, v) = insert p v q
   in foldl insert' empty

empty :: Ord a => PriorityQueue a
empty = PriorityQueue (Set.empty, Map.empty)

isEmpty  :: PriorityQueue a -> Bool
isEmpty (PriorityQueue (s, _)) = Set.null s

priority :: Ord a => PriorityQueue a -> a -> Maybe Int
priority (PriorityQueue (_, m)) v = Map.lookup v m

singleton :: Ord a => Int -> a -> PriorityQueue a
singleton p v = fromList [(p,v)]
