module Learning.PDDL.ConditionalKnowledgeSpec where

import           Learning.PDDL.ConditionalKnowledge

import           Logic.Formula
import           Planning.PDDL

import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Set                           (Set, fromList)
import qualified Data.Set                           as Set

import           Test.Hspec


conditionalKnowledgeSpec :: Spec
conditionalKnowledgeSpec = do
    describe "Merging hypergraphs" $ do
      let q, p, g :: [Vertex Int]
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
        in it "can merge h1 and h2 into h'" $
            (h1 `merge` h2) `shouldBe` h'

    describe "hypergraph from Transition with one predicate type" $ do
        -- Expected hypergraph:
        -- o1: 2,p1      0,p1 - 1,p2      ¬5,p2
        --      |                           |
        -- o2: 3,p2     ¬6,p1 - ¬7,p2     ¬4,p1     8,p1 - 9,p2
        let pn = "p"
            o1 = "o1"
            o2 = "o2"
            objs = [o1, o2]
            predSpec = [Predicate pn [("x",baseType),("y",baseType)]]
            peSpec = PDDLEnvSpec predSpec [] objs Map.empty
            s0 = Set.fromList [ Predicate pn [o1, o2]
                              , Predicate pn [o1, o1]
                              ]
            s1 = s0 `Set.union`
                 Set.fromList [ Predicate pn [o2, o2]
                              ]
            trans = (s0, ("a", []), s1)
            hyp = fromTransition peSpec trans
            hyp' = map snd hyp
            hyp'' = head hyp'
            predicateEdges = Set.filter (isEdgeOfType PredicateEdge) hyp''
            bindingEdges =  Set.filter (isEdgeOfType BindingEdge) hyp''
         in do
           it "can get correct predicate edges" $ do
            let expectedPredicateEdges =
                  fromList [ Edge PredicateEdge
                                (fromList [ Vertex 0 (Precond,Pos "p",1)
                                          , Vertex 1 (Precond,Pos "p",2)
                                          ])
                           , Edge PredicateEdge
                                (fromList [ Vertex 2 (Precond,Pos "p",1)
                                          , Vertex 3 (Precond,Pos "p",2)])
                           , Edge PredicateEdge
                                (fromList [ Vertex 4 (Precond,Neg "p",1)
                                          , Vertex 5 (Precond,Neg "p",2)])
                           , Edge PredicateEdge
                                (fromList [ Vertex 6 (Precond,Neg "p",1)
                                          , Vertex 7 (Precond,Neg "p",2)])
                           , Edge PredicateEdge
                                (fromList [ Vertex (-2) (Effect,Pos "p",1)
                                          , Vertex (-1) (Effect,Pos "p",2)])
                           ]
             in expectedPredicateEdges `shouldBe` predicateEdges
           it "can get correct binding edges" $ do
            let expectedBindingEdges =
                  fromList [ Edge BindingEdge
                                (fromList [ Vertex 0 (Precond,Pos "p",1)
                                          , Vertex 1 (Precond,Pos "p",2)
                                          , Vertex 2 (Precond,Pos "p",1)
                                          , Vertex 5 (Precond,Neg "p",2)])
                           , Edge BindingEdge
                                (fromList [ Vertex 3 (Precond,Pos "p",2)
                                          , Vertex 4 (Precond,Neg "p",1)
                                          , Vertex 6 (Precond,Neg "p",1)
                                          , Vertex 7 (Precond,Neg "p",2)

                                          , Vertex (-2) (Effect,Pos "p",1)
                                          , Vertex (-1) (Effect,Pos "p",2)])
                           ]
             in bindingEdges `shouldBe` expectedBindingEdges


spec :: Spec
spec = conditionalKnowledgeSpec

main :: IO ()
main = hspec spec
