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

import           Control.Arrow                       ((***), second)
import           Control.Monad                       (liftM, replicateM,
                                                      sequence)
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

type HyperGraph a = Set (Edge a)

-- Move this elsewhere
type TotalState = Set (Literal GroundedPredicate)

type EdgeSet a = Set (Vertex a)

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
data Edge a = Edge EdgeType (EdgeSet a) deriving (Eq, Ord, Show)

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
               -> [(Literal Name, HyperGraph Int)]
fromTransition = fromTransitionWithId 0

fromTransitionWithId :: (Num idType, Enum idType, Ord idType)
          => idType
          -> [PredicateSpec]
          -> [Object]
          -> Transition
          -> [(Literal Name, HyperGraph idType)]
fromTransitionWithId initId specs objs  (s,_,s') = map effToHypergraph effData where
  negPre = Set.toList $ universeState specs objs \\ s
  posPre = Set.toList s

  posEff = Set.toList $ s' \\ s
  negEff = Set.toList $ s \\ s'

  split pt lt preds = map (toPredInfo pt lt) preds

  idPred idStart p = zip [idStart..] p

  lengthAsId l = fromInteger (toInteger (length l))

  idPreds idStart (p : rest) = idPred idStart p : idPreds (idStart + lengthAsId p) rest
  idPreds _ [] = []

  toNode (idv, (_, pinfo)) = Vertex idv pinfo
  toNodeBind (idv, (b, pinfo)) = (b, Vertex idv pinfo)

  groupBindings elems = Map.elems
                      $ Map.fromListWith (++) [(k, [v]) | (k, v) <- elems]

  posPreInitId = initId
  posPreNodeData = idPreds posPreInitId $ split Precond Pos posPre
  posPreNodeBindings = concatMap (map toNodeBind) posPreNodeData



  negPreInitId = posPreInitId + lengthAsId posPreNodeBindings
  negPreNodeData = idPreds negPreInitId $ split Precond Neg negPre
  negPreNodeBindings = concatMap (map toNodeBind) negPreNodeData

  effInitId = negPreInitId + lengthAsId negPreNodeBindings
  effData = (zip (map (Pos . predName) posEff) $ split Effect Pos posEff)
          ++ (zip (map (Neg . predName) negEff) $ split Effect Neg negEff)

  predEdges nodeData = map ((Edge PredicateEdge) . Set.fromList)
                     $ map (map toNode) nodeData
  bindingEdges binding = map ((Edge BindingEdge) . Set.fromList)
                       $ groupBindings binding

  prePredEdges =  predEdges posPreNodeData
               ++ predEdges negPreNodeData
  preNodeBindings = posPreNodeBindings
                  ++  negPreNodeBindings

  effToHypergraph (litname, pEffData) =
    let vData = idPred effInitId pEffData
        pEdges = [Edge PredicateEdge $ Set.fromList $ map toNode vData]
               ++ prePredEdges
        bEdges = bindingEdges $ map toNodeBind vData ++ preNodeBindings
     in (litname, Set.fromList $ pEdges ++ bEdges)

edgeSet :: Ord a => Edge a -> EdgeSet a
edgeSet (Edge _ e) = e

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

intersect :: Ord a => EdgeSet a -> EdgeSet a -> Set (Vertex (a, a))
intersect e1 e2 = Set.fromList
                $ catMaybes [ newId v1 v2
                            | v1 <- Set.toList e1
                            , v2 <- Set.toList e2
                            , v1 `similarTo` v2
                            ]

edgeMember :: Ord a => Vertex a -> Edge a -> Bool
edgeMember v e = Set.member v (edgeSet e)

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
    e'     = edgeSet e1 `intersect` edgeSet e2
    h''    = Set.insert (Edge et e') h'
    invs   = [ newIdInv v | v <- Set.toList e', not $ isInEdge et h' v ]
    oldes  = map (containingEdge et' h1 *** containingEdge et' h2) invs
    valids = [ (e1', e2') | (Just e1', Just e2') <- oldes ]
    rest   = foldl (\ hh (e1', e2') -> mergeEdges h1 h2 hh e1' e2') h'' valids

-- | Construct all possible predicates given predicate specification from a
--   domain and objects from a problem.
allGroundedPredicates :: PDDLEnvSpec -> Set GroundedPredicate
allGroundedPredicates eSpec =
    Set.fromList $ concatMap fromSpec $ envsPredSpecs eSpec where
        fromSpec :: PredicateSpec -> [GroundedPredicate]
        fromSpec (Predicate pn pa) = [ Predicate pn p
                                     | p <- replicateM (length pa) (envsObjs eSpec)
                                     ]

-- | Constructs a set of grounded predicates not occuring in the given state
notInState :: State -> PDDLEnvSpec -> State
notInState s eSpec = allGroundedPredicates eSpec \\ s

totalState :: State -> PDDLEnvSpec -> TotalState
totalState s eSpec = Set.map Pos s `Set.union` Set.map Neg ns where
    ns = notInState s eSpec

toPredEdge :: Literal GroundedPredicate -> PType -> Int -> Edge (Object, Int)
toPredEdge lgp pt n = e where
    Predicate nm args = atom lgp
    e = Edge PredicateEdge $ Set.fromList vertList
    vertList = zipWith3 mkVertex [1 ..] args [n ..]
    mkVertex n' a m = Vertex (a, m) (pt, nm `signAs` lgp, n')

-- | Construct the binding edges for a hypergraph based on a list of predicate
--   edges that have already been constructed, and are carrying information
--   specifying which object they represent.
mkBindingEdges :: Ord a => [Edge (Object, a)] -> Set (Edge a)
mkBindingEdges pes = bes where
    -- vs :: Ord a => [Vertex (Object, a)]
    vs   = concatMap (Set.toList . edgeSet) pes
    vMap =  Map.fromListWith Set.union $ map (second Set.singleton . tupleForm) vs
    -- tupleForm :: Vertex (Object, a) -> (Object, Vertex a)
    tupleForm (Vertex (o, i) pinfo) = (o, Vertex i pinfo)
    bes  = Set.fromList $ map (Edge BindingEdge) $ Map.elems vMap

-- | Constructs a hyper graph from a state and an effect (a grounded predicate)
fromState :: PDDLEnvSpec
          -> TotalState                -- ^ The state the action was executed in
          -> Literal GroundedPredicate -- ^ An effect of the action to
                                       --   construct a hypergraph from
          -> HyperGraph Int
fromState espec s lgp = undefined where
    -- effEdge = toPredEdge lgp Effect 1
    -- predPreds = foldl

-- fromTransition :: PDDLEnvSpec
--                -> Transition
--                -> [(Literal Name, HyperGraph Int)]
-- fromTransition = undefined
