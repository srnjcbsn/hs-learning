module Graph.Search.Astar (search, searchBounded) where
import Graph.Search
import Graph
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
--import Data.Set (Set)
import Data.PriorityQueue as PrioQ
import Data.Maybe (mapMaybe,fromMaybe)
import Debug.Trace
import Control.Monad (liftM)

--type SearchNode v e = (v,[e])


updateFrontier :: (ForwardSearchGraph g v e)
               => g
               -> Set v
               -> (PriorityQueue v, Map v (Int, [e]))
               -> (v, Int, [e])
               -> (PriorityQueue v, Map v (Int, [e]))
updateFrontier graph explored frontier@(queue,vMap) (v, cost, path) =
  let isExplored = Set.member v explored
      oldPriority = PrioQ.priority queue v
      newPriority = cost + heuristicCostToGoal graph v
      addedToFrontier =
        ( PrioQ.insert newPriority v queue
        , Map.insert v (cost, path) vMap )
   in case (isExplored, oldPriority) of
        (False, Nothing) -> addedToFrontier
        (_, Just p) | p > newPriority -> addedToFrontier
        _ -> frontier

astar :: (ForwardSearchGraph g v e)
      => g
      -> Set v
      -> Maybe Int
      -> (PriorityQueue v, Map v (Int, [e]))
      -> Maybe [e]
astar graph explored maxDepth (queue,vMap) =
  do (v, q') <- PrioQ.dequeueMin queue
     (cost, path) <- Map.lookup v vMap
     if goalReached graph v || fromMaybe False (liftM (== length path) maxDepth)
     then return $ reverse path
     else
       let newFrontier = (q', Map.delete v vMap)
           edges = adjacentEdges graph v
           etoN e = do adjV <- adjacentVertex graph v e
                       return (adjV, cost + edgeCost graph adjV e, e:path)
           nodes = mapMaybe etoN edges
           frontier = foldl (updateFrontier graph explored) newFrontier nodes
        in  astar graph (Set.insert v explored) maxDepth frontier

internalSearch :: (ForwardSearchGraph graph vertex edge)
       => graph -> vertex -> Maybe Int -> Maybe [edge]
internalSearch graph initV maybeDepth = astar graph explored maybeDepth (queue, vMap)
  where
    explored = Set.empty
    queue = PrioQ.singleton 0 initV
    vMap = Map.singleton initV (0, [])

-- | Search until the plan reaches a certain length then returns that plan
searchBounded :: (ForwardSearchGraph graph vertex edge)
       => graph -> vertex -> Int -> Maybe [edge]
searchBounded graph initV maxDepth = internalSearch graph initV (Just maxDepth)

search :: (ForwardSearchGraph graph vertex edge)
       => graph -> vertex -> Maybe [edge]
search graph initV = internalSearch graph initV Nothing
