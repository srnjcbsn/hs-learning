module Learning.PDDL.ConditionalKnowledge
    (
    -- * Vertices
      Vertex (..)
    , PredInfo
    , PType (..)
    , identifier
    , newId
    , newIdInv
    , similarTo

    -- * Edges
    , Edge (..)
    , EdgeType (..)
    , VertexSet
    , binding
    , vertexSet
    , edgeType
    , isEdgeOfType
    , isEffect
    , otherEdgeType
    , intersect
    , edgeMember

    -- * HyperGraphs
    , HyperGraph
    , merge
    , mergeEdges
    , simplifyHyperGraph

    -- ** Construction
    , toPredEdge
    , mkBindingEdges
    , fromTotalState
    , updateBindings
    , fromEffect
    , fromTransition

    -- ** Query
    , containingEdges
    , containingEdge
    , getEffectP

    ) where

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

import           Control.Arrow                       (second, (***))
import           Control.Monad                       (liftM, replicateM,
                                                      sequence)
import           Data.List                           (mapAccumL)
import qualified Data.List                           as List
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (catMaybes, fromMaybe,
                                                      isJust, listToMaybe)
import           Data.Set                            (Set, (\\))
import qualified Data.Set                            as Set
import           Data.Tree
import           Data.TupleSet                       (TupleSet)
import qualified Data.TupleSet                       as TSet

type Cand = Set (PredInfo, PredInfo)

data CondEffKnowledge = CondEffKnowledge
    { cekHyperGraph :: HyperGraph Int
    , cekProven     :: Set PredInfo
    , cekDisproven  :: Set PredInfo
    , cekCands      :: Set Cand
    }

type CondActionKnowledge = Map (Literal FluentPredicate) CondEffKnowledge

type CondDomainKnowledge = Map Name CondActionKnowledge

mkCands :: HyperGraph Int -- ^ The existing hypergraph for the failed effect.
        -> HyperGraph Int -- ^ The hyper graph constructed for the failed effect.
        -> Cand
mkCands unknHg failHg = undefined

mergeCandEdges :: Ord a
           => HyperGraph a
           -> HyperGraph a
           -> HyperGraph (a, a)
           -> Edge a
           -> Edge a
           -> (HyperGraph (a, a), [(PredInfo, PredInfo)])
mergeCandEdges h1 h2 h' e1 e2 = second ((++ misses) . concat) rest where
    et     | edgeType e1 == edgeType e2 = edgeType e1
           | otherwise = error "can not merge predicate and binding edges"
    et'    = otherEdgeType et
    e'     = vertexSet e1 `intersect` vertexSet e2
    misses = vertexSet e1 `difference` vertexSet e2
    h''    = Set.insert (Edge et e') h'
    invs   = [ newIdInv v | v <- Set.toList e', not $ isInEdge et h' v ]
    oldes  = map (containingEdge et' h1 *** containingEdge et' h2) invs
    valids = [ (e1', e2') | (Just e1', Just e2') <- oldes ]
    rest   = mapAccumL (\hh (e1', e2') -> mergeCandEdges h1 h2 hh e1' e2') h'' valids

updateEffectKnowledge :: CondEffKnowledge
                      -> Literal GroundedPredicate
                      -> TotalState
                      -> CondEffKnowledge
updateEffectKnowledge effMap lgp s = undefined

updateActionKnowledge :: CondActionKnowledge
                      -> Transition
                      -> CondActionKnowledge
updateActionKnowledge actMap (s, _, s') = undefined

updateDomainKnowledge :: CondDomainKnowledge
                      -> PDDLDomain
                      -> PDDLProblem
                      -> Transition
                      -> CondDomainKnowledge
updateDomainKnowledge knl dom prob t@(s, a, s') =
    case Map.lookup (aName a) knl of
      Just actKnl -> Map.insert (aName a) (updateActionKnowledge actKnl t) knl
      Nothing     -> error $  "updateDomainKnowledge: failed to find action "
                           ++ aName a ++ " in knowledge map."

type HyperGraph a = Set (Edge a)

type VertexSet a = Set (Vertex a)

-- | Information about which index of which predicate a vertex represents,
--   and whether that predicate was an effect or a precondition.
type PredInfo = (PType, Literal Name, Int)

-- | A 'Vertex' consists of a unique identifier and a description of which
--   predicate argument it represents
data Ord a => Vertex a = Vertex a PredInfo deriving (Eq, Ord, Show)

-- | The type of a predicate, denoting whether it describes an effect or a
--   precondition.
data PType = Effect
           | Precond
           deriving (Eq, Ord, Show)

-- | An edge set in a hyper graph can either be a binding edge or a predicate
--   edge.
data EdgeType = BindingEdge
              | PredicateEdge
              deriving (Eq, Ord, Show)

-- | An edge is an 'EdgeType' and a collection of vertices.
data Edge a = Edge EdgeType (VertexSet a) deriving (Eq, Ord, Show)

identifier :: Ord a => Vertex a -> a
identifier (Vertex a _) = a

-- | Given a binding edge, returns the object that the edge represents.
--   The vertices in the edge must contain this information in ther identifiers.
binding :: Ord a => Edge (Object, a) -> Object
binding (Edge BindingEdge bvs) =
    case Set.toList bvs of
      ( v : _ ) -> fst $ identifier v
      []        -> error "binding: Empty binding set encountered"
binding _ = error "binding: A predicate edge does not have a common binding"

edgesOfType :: Ord a => EdgeType -> HyperGraph a -> Set (Edge a)
edgesOfType t = Set.filter (isEdgeOfType t)

bindingEdges, predicateEdges :: Ord a => HyperGraph a -> Set (Edge a)
bindingEdges   = edgesOfType BindingEdge
predicateEdges = edgesOfType PredicateEdge

-- | Update the binding edges in the given 'HyperGraph' to contain the vertices
--   in the given 'VertexSet' (if appropriate).
updateBindings :: HyperGraph (Object, Int)
               -> VertexSet (Object, Int)
               -> HyperGraph (Object, Int)
updateBindings hg verts = Set.map upd hg where
    upd :: Edge (Object, Int) -> Edge (Object, Int)
    upd e@(Edge BindingEdge bvs) = Edge BindingEdge (sameVerts e `Set.union` bvs)
    upd e = e
    sameVerts :: Edge (Object, Int) -> VertexSet (Object, Int)
    sameVerts e = Set.filter (\v -> fst (identifier v) == binding e) verts

-- | Add a effect predicate edge describing the given signed predicate to the
--   hyper graph, and update the binding edges to incorporate the vertices in
--   the newly formed edge.
fromEffect :: HyperGraph (Object, Int)
           -> Literal GroundedPredicate
           -> HyperGraph Int
fromEffect hg lgp = simplifyHyperGraph
                  $ Set.insert eff
                  $ updateBindings hg (vertexSet eff)
    where eff = toPredEdge lgp Effect (-1 * (predArity . atom) lgp)

-- | Remove annotations regarding objects from all vertices of the given hyper
--   graph (they are only relevant for a single state transition).
simplifyHyperGraph :: HyperGraph (Object, Int) -> HyperGraph Int
simplifyHyperGraph hg = Set.map simplifyEdge hg where
    simplifyEdge (Edge t vs) = Edge t $ Set.map simplifyVertex vs
    simplifyVertex (Vertex (_, i) pinfo) = Vertex i pinfo

-- | Generate the base hyper graph from a state.
fromTotalState :: PDDLEnvSpec -> TotalState -> HyperGraph (Object, Int)
fromTotalState peSpec ts = bes `Set.union` pes where
    pes = Set.fromList $ snd $ mapAccumL toPredEdge' 0 (Set.toList ts)
    toPredEdge' :: Int -> Literal GroundedPredicate -> (Int, Edge (Object, Int))
    toPredEdge' n lgp = (n + (predArity . atom) lgp, toPredEdge lgp Precond n)
    bes = mkBindingEdges (Set.toList pes)

-- | Computes all hyper graphs for a single transition. Returns an association
--   list of effects and corresponding hyper graphs.
fromTransition :: PDDLEnvSpec
               -> Transition
               -> [(Literal GroundedPredicate, HyperGraph Int)]
fromTransition peSpec (s, _, s') = Set.toList hgs where
    ts = totalState s peSpec
    ts' = totalState s' peSpec \\ ts -- For now, only consider delta state
    hgBase = fromTotalState peSpec ts
    hgs = Set.map (\lgp -> (lgp, fromEffect hgBase lgp)) ts'

vertexSet :: Ord a => Edge a -> VertexSet a
vertexSet (Edge _ e) = e

edgeType :: Ord a => Edge a -> EdgeType
edgeType (Edge et _) = et

isEdgeOfType :: Ord a => EdgeType ->  Edge a -> Bool
isEdgeOfType et = (== et) . edgeType

otherEdgeType :: EdgeType -> EdgeType
otherEdgeType PredicateEdge = BindingEdge
otherEdgeType BindingEdge   = PredicateEdge

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

difference :: Ord a => VertexSet a -> VertexSet a -> [(PredInfo, PredInfo)]
difference e1 e2 = undefined -- Set.fromList

intersect :: Ord a => VertexSet a -> VertexSet a -> Set (Vertex (a, a))
intersect e1 e2 = Set.fromList
                $ catMaybes [ newId v1 v2
                            | v1 <- Set.toList e1
                            , v2 <- Set.toList e2
                            , v1 `similarTo` v2
                            ]

edgeMember :: Ord a => Vertex a -> Edge a -> Bool
edgeMember v e = Set.member v (vertexSet e)

isInEdge :: Ord a => EdgeType -> HyperGraph a -> Vertex a -> Bool
isInEdge et hg v = isJust (containingEdge et hg v)

containingEdges :: Ord a => HyperGraph a -> Vertex a -> [Edge a]
containingEdges hg v = filter (edgeMember v) $ Set.toList hg

containingEdge :: Ord a
               => EdgeType
               -> HyperGraph a
               -> Vertex a
               -> Maybe (Edge a)
containingEdge et hg v = listToMaybe
                       $ filter (\e -> edgeMember v e && edgeType e == et)
                       $ Set.toList hg

merge :: Ord a => HyperGraph a -> HyperGraph a -> HyperGraph (a, a)
merge h1 h2 = mergeEdges h1 h2 Set.empty (getEffectP h1) (getEffectP h2)

mergeEdges :: Ord a
           => HyperGraph a
           -> HyperGraph a
           -> HyperGraph (a, a)
           -> Edge a
           -> Edge a
           -> HyperGraph (a, a)
mergeEdges h1 h2 h' e1 e2 = rest where
    et     | edgeType e1 == edgeType e2 = edgeType e1
           | otherwise = error "can not merge predicate and binding edges"
    et'    = otherEdgeType et
    e'     = vertexSet e1 `intersect` vertexSet e2
    h''    = Set.insert (Edge et e') h'
    invs   = [ newIdInv v | v <- Set.toList e', not $ isInEdge et h' v ]
    oldes  = map (containingEdge et' h1 *** containingEdge et' h2) invs
    valids = [ (e1', e2') | (Just e1', Just e2') <- oldes ]
    rest   = foldl (\ hh (e1', e2') -> mergeEdges h1 h2 hh e1' e2') h'' valids

-- | Transform a signed grounded predicate into a predicate edge.
--   Each vertex in the edge will be have a unique identifier consisting of
--   a unique integer, as well as the object it represents.
toPredEdge :: Literal GroundedPredicate
           -> PType
           -> Int                       -- ^ The starting integer to use for
                                        --   unique identifiers.
           -> Edge (Object, Int)
toPredEdge lgp pt n = e where
    Predicate nm args = atom lgp
    e = Edge PredicateEdge $ Set.fromList vertList
    vertList = zipWith3 mkVertex [1 ..] args [n ..]
    mkVertex n' a m = Vertex (a, m) (pt, nm `signAs` lgp, n')

-- | Construct the binding edges for a hypergraph based on a list of predicate
--   edges that have already been constructed, and are carrying information
--   specifying which object they represent.
mkBindingEdges :: Ord a => [Edge (Object, a)] -> Set (Edge (Object, a))
mkBindingEdges pes = bes where
    -- vs :: Ord a => [Vertex (Object, a)]
    vs   = concatMap (Set.toList . vertexSet) pes
    vMap =  Map.fromListWith Set.union $ map (second Set.singleton . tupleForm) vs
    -- tupleForm :: Vertex (Object, a) -> (Object, Vertex a)
    tupleForm (Vertex (o, i) pinfo) = (o, Vertex (o, i) pinfo)
    bes  = Set.fromList $ map (Edge BindingEdge) $ Map.elems vMap


