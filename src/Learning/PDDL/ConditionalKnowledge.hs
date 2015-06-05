module Learning.PDDL.ConditionalKnowledge where

import           Data.TupleSet
import           Data.Typeable
import           Environment
import qualified Learning                            as Lrn
import qualified Learning.Induction                  as Ind
import qualified Learning.PDDL                       as PDDL
import qualified Learning.PDDL.EffectKnowledge       as Eff
import qualified Learning.PDDL.PreconditionKnowledge as Pre
import           Logic.Formula
import           Planning
import           Planning.PDDL

import           Control.Monad                       (sequence, replicateM, liftM)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (isJust, fromMaybe, catMaybes, listToMaybe)
import           Data.Set                            (Set, (\\))
import qualified Data.Set                            as Set
import           Data.Tree
import           Data.TupleSet                       (TupleSet)
import qualified Data.TupleSet                       as TSet
import qualified Data.List as List
import Control.Arrow ((***))

data Literal a = Pos a
               | Not a
               deriving (Eq, Ord, Show)

type EdgeSet a = Set (Vertex a)

type PredInfo = (PType, Literal Name, Int)

data Ord a => Vertex a = Vertex a PredInfo deriving (Eq, Ord)

data PType = Effect 
           | Precond
           deriving (Eq, Ord)

data EdgeType = BindingEdge
              | PredicateEdge
              deriving (Eq, Ord)

data Edge a = Edge EdgeType (EdgeSet a)

-- data Ord a => Edge a = BindingEdge         (EdgeSet a) 
--                      | PredicateEdge  (EdgeSet a)
--                      deriving (Eq, Ord)

edgeSet :: Ord a => Edge a -> EdgeSet a
edgeSet (Edge _ e) = e

edgeType :: Ord a => Edge a -> EdgeType
edgeType (Edge et _) = et

-- edgeSet :: Ord a => Edge a -> EdgeSet a
-- edgeSet (BindingEdge e)   = e
-- edgeSet (PredicateEdge e) = e

-- | An edge is an effect edge if it is a predicate edge and contains a vertex
--   marked as an effect.
isEffect :: Ord a => Edge a -> Bool
isEffect (Edge PredicateE e) = not $ null [ () | Vertex _ (Effect, _, _) <- Set.toList e ]
isEffect _ = False

type HyperGraph a = Set (Edge a)

getEffectP :: Ord a => HyperGraph a -> Edge a
getEffectP hg = fromMaybe (error "could not find effect in hyper graph") 
              $ List.find isEffect (Set.toList hg)

newId :: Ord a => Vertex a -> Vertex a -> Maybe (Vertex (a, a))
newId (Vertex i n) (Vertex j m) | n == m    = Just (Vertex (i, j) n)
                                | otherwise = Nothing

newIdInv :: Ord a => Vertex (a, a) -> (Vertex a, Vertex a)
newIdInv (Vertex (i, j) n) = (Vertex i n, Vertex j n)

similarTo :: Ord a => Vertex a -> Vertex a -> Bool
similarTo v1 v2 = isJust $ newId v1 v2

intersect :: Ord a => EdgeSet a -> EdgeSet a -> [Vertex (a, a)]
intersect e1 e2 = catMaybes [ newId v1 v2 | v1 <- Set.toList e1, v2 <- Set.toList e2, v1 `similarTo` v2 ]

merge :: HyperGraph a -> HyperGraph a -> HyperGraph a
merge h1 h2 = undefined

edgeMember :: Ord a => Vertex a -> Edge a -> Bool
edgeMember v e = Set.member v (edgeSet e)

isInBindingEdge :: Ord a => HyperGraph a -> Vertex a -> Bool
isInBindingEdge hg = isJust . (containingBindingEdge hg)

isInPredicateEdge :: Ord a => HyperGraph a -> Vertex a -> Bool
isInPredicateEdge hg = isJust . (containingPredicateEdge hg)

containingEdges :: Ord a => HyperGraph a -> Vertex a -> [Edge a]
containingEdges hg v = filter (edgeMember v) $ Set.toList hg

containingPredicateEdge :: Ord a => HyperGraph a -> Vertex a -> Maybe (Edge a)
containingPredicateEdge hg v = listToMaybe [ PredicateEdge e | PredicateEdge e <- containingEdges hg v ]

containingBindingEdge :: Ord a => HyperGraph a -> Vertex a -> Maybe (Edge a)
containingBindingEdge hg v = listToMaybe [ BindingEdge e | BindingEdge e <- containingEdges hg v ]

mergeEdges :: Ord a
           => HyperGraph a
           -> HyperGraph a
           -> HyperGraph (a, a)
           -> Edge a
           -> Edge a
           -> HyperGraph (a, a)
mergeEdges h1 h2 h' e1 e2 = undefined where
    int    = edgeSet e1 `intersect` edgeSet e2

mergeEdges :: Ord a 
           => HyperGraph a
           -> HyperGraph a
           -> HyperGraph (a, a)
           -> Edge a 
           -> Edge a
           -> HyperGraph (a, a)
mergeEdges h1 h2 h' (PredicateEdge e1) (PredicateEdge e2) = rest where
    int    = edgeIntersection e1 e2
    intSet = PredicateEdge $ Set.fromList int
    h''    = Set.insert intSet h'
    invs   = [ newIdInv v | v <- int, not $ isInBindingEdge h' v ]
    oldes  = map ((containingBindingEdge h1) *** (containingBindingEdge h2)) invs
    valids = [ (e1', e2') | (Just e1', Just e2') <- oldes ]
    rest   = foldl (\hh (e1', e2') -> mergeEdges h1 h2 hh e1' e2') h'' valids
mergeEdges h1 h2 h' (BindingEdge e1) (BindingEdge e2) =
    undefined

