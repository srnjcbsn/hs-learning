module Graph.Search.AstarSpec (main, spec) where

import           Data.List          (sort)
import           Data.Maybe         (mapMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Graph
import           Graph.Search
import           Graph.Search.Astar
import           Test.Hspec

newtype MockGraph = MockGraph (Set (Int,Int),Int -> Bool)

instance Graph MockGraph Int (Int,Int) where
  adjacentEdges = neighbours
  adjacentVertex (MockGraph (g,_)) v e@(v1,v2)
    | Set.member e g && v == v1 = Just v2
    | Set.member e g && v == v2 = Just v1
    | otherwise = Nothing
  edgeCost _ _ _ = 1

instance ForwardSearchGraph MockGraph Int (Int,Int) where
  goalReached (MockGraph (_,goal)) = goal
  heuristicCostToGoal (MockGraph (_,_)) _ = 0

create :: (Int -> Bool) -> MockGraph
create g = MockGraph (Set.empty, g)

addEdge :: (Int,Int) -> MockGraph -> MockGraph
addEdge (n1,n2) (MockGraph (graph,goal)) =
  let [a1, a2] = sort [n1, n2]
   in MockGraph (Set.insert (a1, a2) graph,goal)

ofList :: [(Int,Int)] -> (Int -> Bool) -> MockGraph
ofList l goal =
    let graph = create goal
     in foldl (flip addEdge) graph l

neighbours ::  MockGraph -> Int -> [(Int,Int)]
neighbours (MockGraph (graph,_)) n =
    mapMaybe (\edge ->
                    case edge of
                     e@(n1,_) | n == n1 -> Just e
                     e@(_,n2) | n == n2 -> Just e
                     _ -> Nothing
                ) $ Set.toList graph



testAstarSpec :: Spec
testAstarSpec = do
    describe "search" $ do
      it "can find the correct path in graph with 2 paths" $
        let edges = [   (1,2), (1,5), (5,6), (6,7)
                    ,   (2,3)
                    ,   (3,4)
                    ,   (4,9)
                    ,   (9,10)
                    ,   (10,11)
                    ,   (11,12)
                    ]
            startNode = 1
            goalNode = 12
            goal = (== goalNode)
            graph = ofList edges goal
            actualPath = search graph startNode
            expectedPath =  [   (1,2)
                            ,   (2,3)
                            ,   (3,4)
                            ,   (4,9)
                            ,   (9,10)
                            ,   (10,11)
                            ,   (11,12)
                            ]
         in actualPath `shouldBe` Just expectedPath


spec :: Spec
spec = testAstarSpec

main :: IO ()
main = hspec spec
