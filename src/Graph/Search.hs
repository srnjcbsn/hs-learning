module Graph.Search where
import Graph

class Graph g v e => ForwardSearchGraph g v e | g -> v e where
    goalReached :: g -> v -> Bool
    heuristicCostToGoal :: g -> v -> Int
