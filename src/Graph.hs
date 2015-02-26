module Graph where

class Ord v => Graph g v e | g -> v e where
  adjacentEdges :: g -> v -> [e]
  adjacentVertex :: g -> v -> e -> v
  edgeCost :: g -> v -> e -> Int
  adjacentVertices :: g -> v -> [v]
  adjacentVertices g v = map (adjacentVertex g v) $ adjacentEdges g v
