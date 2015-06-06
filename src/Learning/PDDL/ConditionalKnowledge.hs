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

import           Control.Arrow                       ((***))
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

data Literal a = Pos a
               | Not a
               deriving (Eq, Ord, Show)

instance Functor Literal where
    fmap f (Pos a) = Pos (f a)
    fmap f (Not a) = Not (f a)

-- | Pack 'b' into a literal with same sign as 'a'
signAs :: b -> Literal a -> Literal b
signAs b = fmap (const b)

-- | Extract the atom of a 'Literal', throwing aeay the sign
atom :: Literal a -> a
atom (Pos a) = a
atom (Not a) = a

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

edgeSet :: Ord a => Edge a -> EdgeSet a
edgeSet (Edge _ e) = e

edgeType :: Ord a => Edge a -> EdgeType
edgeType (Edge et _) = et

otherEdgeType :: EdgeType -> EdgeType
otherEdgeType PredicateEdge = BindingEdge
otherEdgeType BindingEdge   = PredicateEdge

-- | An edge is an effect edge if it is a predicate edge and contains a vertex
--   marked as an effect.
isEffect :: Ord a => Edge a -> Bool
isEffect (Edge PredicateEdge e) = not $ null [ () | Vertex _ (Effect, _, _) <- Set.toList e ]
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
    et     = if edgeType e1 == edgeType e2 then edgeType e1
             else error "can not merge predicate and binding edges"
    et'    = otherEdgeType et
    e'     = edgeSet e1 `intersect` edgeSet e2
    h''    = Set.insert (Edge et e') h'
    invs   = [ newIdInv v | v <- Set.toList e', not $ isInEdge et h' v ]
    oldes  = map (containingEdge et' h1 *** containingEdge et' h2) invs
    valids = [ (e1', e2') | (Just e1', Just e2') <- oldes ]
    rest   = foldl (\ hh (e1', e2') -> mergeEdges h1 h2 hh e1' e2') h'' valids

literalName :: Literal (Predicate a) -> Literal Name
literalName (Pos (Predicate nm _)) = Pos nm
literalName (Not (Predicate nm _)) = Pos nm

toPredEdge :: Literal GroundedPredicate -> PType -> Int -> Edge Int
toPredEdge lgp@(Pos (Predicate nm args)) pt n =  
    Edge PredicateEdge $ Set.fromList vertList where 
        vertList = zipWith mkVertex [1 .. length args] [n ..]
        mkVertex n' m = Vertex m (pt, nm `signAs` lgp, n')

-- | Construct all possible predicates given predicate specification from a 
--   domain and objects from a problem.
allGroundedPredicates :: PDDLDomain -> PDDLProblem -> Set GroundedPredicate
allGroundedPredicates dom prob = 
    Set.fromList $ concatMap fromSpec $ dmPredicates dom where
        fromSpec :: PredicateSpec -> [GroundedPredicate]
        fromSpec (Predicate pn pa) = [ Predicate pn p 
                                     | p <- replicateM (length pn) (probObjs prob)
                                     ] 

-- | Constructs a set of grounded predicates not occuring in the given state
notInState :: State -> PDDLDomain -> PDDLProblem -> State
notInState s dom prob = allGroundedPredicates dom prob \\ s

totalState :: State -> PDDLDomain -> PDDLProblem -> TotalState
totalState s dom prob = Set.map Pos s `Set.union` Set.map Not ns where
    ns = notInState s dom prob

-- | Constructs a hyper graph from a state and an effect (a grounded predicate)
fromState :: TotalState                -- ^ The state the action was executed in
          -> Literal GroundedPredicate -- ^ An effect of the action to 
                                       --   construct a hypergraph from
          -> PDDLProblem 
          -> PDDLDomain 
          -> HyperGraph Int
fromState s lgp prob dom = undefined where
    effEdge = toPredEdge lgp Effect 1

