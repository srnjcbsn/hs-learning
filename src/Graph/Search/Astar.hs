module Graph.Search.Astar (search) where
import Graph.Search
import Graph
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
--import Data.Set (Set)
import Data.PriorityQueue as PrioQ

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
      -> (PriorityQueue v, Map v (Int, [e]))
      -> Maybe [e]
astar graph explored (queue,vMap) =
  do (v, q') <- PrioQ.dequeueMin queue
     (cost, path) <- Map.lookup v vMap
     if goalReached graph v
     then return $ reverse path
     else
       let newFrontier = (q', Map.delete v vMap)
           edges = adjacentEdges graph v
           etoN e =  (adjacentVertex graph v e, cost + edgeCost graph v e, e:path)
           nodes = map etoN edges
           frontier = foldl (updateFrontier graph explored) newFrontier nodes
        in astar graph (Set.insert v explored) frontier

search :: (ForwardSearchGraph graph vertex edge)
       => graph -> Maybe [edge]
search graph = astar graph explored (queue, vMap)
    where
      initV = initialVertex graph
      explored = Set.empty
      queue = PrioQ.singleton 0 initV
      vMap = Map.singleton initV (0, [])




--(initialVertex g, [])
--