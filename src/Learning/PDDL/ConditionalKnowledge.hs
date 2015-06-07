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

type HyperGraph a = Set (Edge a)

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
            deriving (Eq, Ord)

universeState :: [PredicateSpec]
              -> [Object]
              -> State
universeState specs objs = Set.fromList allPreds where
  args = flip replicateM objs
  toPreds (Predicate n a) = map (Predicate n) (args $ length a)
  allPreds = concatMap toPreds specs


toPredInfo :: PType
           -> (Name -> Literal Name)
           -> GroundedPredicate
           -> [(Object,PredInfo)]
toPredInfo ptype litType (Predicate n objs) = map toPInfo infos where
  infos = zip (map ((,) n) objs) [1..]
  toPInfo ((na,obj), pos) = (,) obj (ptype, litType na, pos)


fromTransition :: [PredicateSpec]
          -> [Object]
          -> Transition
          -> [HyperGraph Int]
fromTransition specs objs (s,_,s') = map effToHypergraph effData where
  negPre = Set.toList $ universeState specs objs \\ s
  posPre = Set.toList s

  posEff = Set.toList $ s' \\ s
  negEff = Set.toList $ s \\ s'

  split pt lt preds = map (toPredInfo pt lt) preds

  idPred initId p = zip [initId..] p

  idPreds initId (p :rest) = idPred initId p : idPreds (initId + length p) rest
  idPreds _ [] = []

  toNode (idv, (_, pinfo)) = Vertex idv pinfo
  toNodeBind (idv, (b, pinfo)) = (b, Vertex idv pinfo)

  groupBindings elems = Map.elems
                      $ Map.fromListWith (++) [(k, [v]) | (k, v) <- elems]

  posPreInitId = 0
  posPreNodeData = idPreds posPreInitId $ split Precond Pos posPre
  posPreNodeBindings = concatMap (map toNodeBind) posPreNodeData


  negPreInitId = posPreInitId + length posPreNodeBindings
  negPreNodeData = idPreds negPreInitId $ split Precond Not negPre
  negPreNodeBindings = concatMap (map toNodeBind) negPreNodeData

  effInitId = negPreInitId + length negPreNodeBindings
  effData = split Effect Pos posEff ++ split Effect Not negEff

  predEdges nodeData = map ((Edge PredicateEdge) . Set.fromList)
                     $ map (map toNode) nodeData
  bindingEdges binding = map ((Edge BindingEdge) . Set.fromList)
                       $ groupBindings binding

  prePredEdges =  predEdges posPreNodeData
               ++ predEdges negPreNodeData
  preNodeBindings = posPreNodeBindings
                  ++  negPreNodeBindings

  effToHypergraph pEffData =
    let vData = idPred effInitId pEffData
        pEdges = [Edge PredicateEdge $ Set.fromList $ map toNode vData]
               ++ prePredEdges
        bEdges = bindingEdges $ map toNodeBind vData ++ preNodeBindings
     in Set.fromList $ pEdges ++ bEdges

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
isEffect (Edge PredicateEdge e) = not $ null [ () | Vertex _ (Effect, _, _) <- Set.toList e ]
isEffect _ = False


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
containingPredicateEdge hg v = listToMaybe [ Edge PredicateEdge e | Edge PredicateEdge e <- containingEdges hg v ]

containingBindingEdge :: Ord a => HyperGraph a -> Vertex a -> Maybe (Edge a)
containingBindingEdge hg v = listToMaybe [ Edge BindingEdge e | Edge BindingEdge e <- containingEdges hg v ]



mergeEdges :: Ord a
           => HyperGraph a
           -> HyperGraph a
           -> HyperGraph (a, a)
           -> Edge a
           -> Edge a
           -> HyperGraph (a, a)
mergeEdges h1 h2 h' (Edge PredicateEdge e1) (Edge PredicateEdge e2) = rest where
    int    = intersect e1 e2
    newEdge = Edge PredicateEdge $ Set.fromList int
    h''    = Set.insert newEdge h'
    invs   = [ newIdInv v | v <- int, not $ isInBindingEdge h' v ]
    oldes  = map ((containingBindingEdge h1) *** (containingBindingEdge h2)) invs
    valids = [ (e1', e2') | (Just e1', Just e2') <- oldes ]
    rest   = foldl (\hh (e1', e2') -> mergeEdges h1 h2 hh e1' e2') h'' valids
mergeEdges h1 h2 h' (Edge BindingEdge e1) (Edge BindingEdge e2) =
    undefined
