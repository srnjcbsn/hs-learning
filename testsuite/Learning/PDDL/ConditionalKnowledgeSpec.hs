module Learning.PDDL.ConditionalKnowledgeSpec where

import Learning.PDDL.ConditionalKnowledge

import Data.Set (Set)
import qualified Data.Set as Set

import           Test.Hspec

q, p, g :: [Vertex Int]
q = [ Vertex 1 (Effect, Pos "q", 1)
    , Vertex 2 (Effect, Pos "q", 2)
    ]

p = [ Vertex 3 (Precond, Pos "p", 1)
    , Vertex 4 (Precond, Pos "p", 2)
    ]

g = [ Vertex 5 (Precond, Pos "g", 1)
    , Vertex 6 (Precond, Pos "g", 2)
    ]

q', p', g' :: [Vertex (Int, Int)]
q' = [ Vertex (1, 1) (Effect, Pos "q", 1)
     , Vertex (2, 2) (Effect, Pos "q", 2)
     ]

p' = [ Vertex (3, 3) (Precond, Pos "p", 1)
     , Vertex (4, 4) (Precond, Pos "p", 2)
     ]

g' = [ Vertex (5, 5) (Precond, Pos "g", 1)
     , Vertex (6, 6) (Precond, Pos "g", 2)
     ]

predEdges :: [Edge Int]
predEdges = 
    [ Edge PredicateEdge $ Set.fromList q
    , Edge PredicateEdge $ Set.fromList p
    , Edge PredicateEdge $ Set.fromList g
    ]

h1, h2 :: HyperGraph Int
h1 = Set.fromList $ predEdges ++ 
    [ Edge BindingEdge $ Set.fromList [q !! 0, p !! 0]
    , Edge BindingEdge $ Set.fromList [q !! 1, g !! 0]
    , Edge BindingEdge $ Set.fromList [p !! 1, g !! 1]
    ]

h2 = Set.fromList $ predEdges ++
    [ Edge BindingEdge $ Set.fromList [q !! 0, p !! 0]
    , Edge BindingEdge $ Set.fromList [q !! 1]
    , Edge BindingEdge $ Set.fromList [g !! 0]
    , Edge BindingEdge $ Set.fromList [p !! 1, g !! 1]
    ]

h' :: HyperGraph (Int, Int)
h' = Set.fromList 
    [ Edge PredicateEdge $ Set.fromList q'
    , Edge PredicateEdge $ Set.fromList p'
    , Edge PredicateEdge $ Set.fromList g'
    , Edge BindingEdge   $ Set.fromList [q' !! 0, p' !! 0]
    , Edge BindingEdge   $ Set.fromList [q' !! 1]
    , Edge BindingEdge   $ Set.fromList [g' !! 0]
    , Edge BindingEdge   $ Set.fromList [p' !! 1, g' !! 1]
    ]

conditionalKnowledgeSpec :: Spec
conditionalKnowledgeSpec = do
    describe "Merging hyper graphs" $ do
        it "can merge hyper graphs" $
            (h1 `merge` h2) `shouldBe` h'

spec :: Spec
spec = conditionalKnowledgeSpec

main :: IO ()
main = hspec spec
