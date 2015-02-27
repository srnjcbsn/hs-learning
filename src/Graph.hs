module Graph where
import Data.Maybe (mapMaybe)

class Ord v => Graph g v e | g -> v e where
  adjacentEdges :: g -> v -> [e]
  adjacentVertex :: g -> v -> e -> Maybe v
  edgeCost :: g -> v -> e -> Int
  adjacentVertices :: g -> v -> [v]
  adjacentVertices g v = mapMaybe (adjacentVertex g v) $ adjacentEdges g v
