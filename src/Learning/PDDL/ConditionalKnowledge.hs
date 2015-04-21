module Learning.PDDL.ConditionalKnowledge where

import           Data.TupleSet
import           Data.Typeable
import           Environment
import qualified Learning                            as Lrn
import           Learning.Induction
import qualified Learning.PDDL                       as PDDL
import qualified Learning.PDDL.EffectKnowledge       as Eff
import qualified Learning.PDDL.PreconditionKnowledge as Pre
import           Logic.Formula
import           Planning
import           Planning.PDDL

import           Control.Monad
import           Data.Map                            (Map, (!))
import qualified Data.Map                            as Map
import           Data.Maybe
import           Data.Set                            (Set, (\\))
import qualified Data.Set                            as Set
import           Data.Tree
import qualified Data.TupleSet                       as TSet

type DomainKnowledge = Map Name Pattern

data Binding = Bound Int
             | Free  Int

type Knowledge = TupleSet (Predicate Binding)

data Theory = Theory
    { known   :: Knowledge
    , unknown :: Knowledge
    }

data Cond = Cond
    { precs     :: Theory
    , precCands :: Set Knowledge
    }

type ActionTheory = Map (Predicate Int) Cond

type Subst = Map Object Int

type CArg = Int

data Pattern = Pattern
    { ctEffects  :: PDDL.EffKnowledge CArg
    , ctPreconds :: PDDL.PreKnowledge CArg
    } deriving (Eq, Ord)

type CArgMap = (Map (CArg,CArg) CArg, CArg)

type Combinations = [Map CArg CArg]

combinations :: (Pattern, Pattern) -> CArgMap -> (Combinations, Combinations)
combinations = undefined

unground :: Pattern -> Combinations -> Pattern
unground = undefined



matchPredicates :: CArgMap
                -> Predicate CArg
                -> [[CArg]]
                -> CArgMap
matchPredicates argMap p argLs  =
  let smerge :: [CArg] -> CArgMap -> [CArg] -> CArgMap
      smerge  [] m [] = m
      smerge (h1:rest1)  (m, newCArg) (h2:rest2) =
        let f Nothing = Just newCArg
            f val = val
            newMapping = (h1,h2)
            m' = Map.alter f newMapping m
         in if Map.member newMapping m
            then smerge rest1 (m', newCArg + 1) rest2
            else smerge rest1 (m, newCArg) rest2
      smerge _ _ _ = error ("Differnt arity for " ++ predName p)
   in foldl (smerge $ predArgs p) argMap argLs

group :: Set (Predicate CArg)
      -> Set (Predicate CArg)
      -> [(Predicate CArg,[[CArg]])]
group ps1 ps2 =
    let matched ps p  = (p, map predArgs $ Set.toList $ Set.filter (matcher p) ps )
        matcher p = (== predName p) . predName
     in Set.toList $ Set.map (matched ps2) ps1

mapping :: CArgMap
        -> Set (Predicate CArg)
        -> Set (Predicate CArg)
        -> CArgMap
mapping cArgMap knl1 knl2 =
  let knlgroup = group knl1 knl2
      f m (p,argLs) = matchPredicates m p argLs
   in foldl f cArgMap knlgroup

initMapping :: Set (Predicate CArg)
            -> Set (Predicate CArg)
            -> CArgMap
initMapping = mapping (Map.empty, 0)

-- Only works as long as effects doesn't contain more of each effect IE.
-- p(x,y) p(y,z) <-- Not allowed
-- p(x,y) f(y) <-- Allowed
merge :: Pattern -> Pattern -> Pattern
merge p1 p2 =
  let ek p = PDDL.ekHyp (ctEffects p)
      ekP1 = ek p1
      ekP2 = ek p2

   in undefined

merges :: [Pattern] -> Maybe Pattern
merges [] = Nothing
merges ps = Just $ foldl1 merge ps

type Shape = Predicate Int

shapeOf :: Ord a => Predicate a -> Shape
shapeOf (Predicate pn args) = Predicate pn (shapeOf' args 0 Map.empty)  where
    shapeOf' :: Ord a => [a] -> Int -> Map a Int -> [Int]
    shapeOf' (a : as) maxVal m =
        case Map.lookup a m of
             Just s  -> s : shapeOf' as maxVal m
             Nothing -> maxVal : shapeOf' as (maxVal + 1) m' where
                        m' = (Map.insert a (maxVal + 1) m)
    shapeOf' [] _ _ = []

emptyHyp :: Ord a => PDDL.Hyp a
emptyHyp = PDDL.Hyp TSet.empty TSet.empty

emptyPreKnl :: Ord a => PDDL.PreKnowledge a
emptyPreKnl = PDDL.PreKnowledge emptyHyp Set.empty

fromTransition :: Pattern
               -> Transition
               -> [PredicateSpec]
               -> [Object]
               -> Pattern

fromTransition patt (s, _, s') pSpecs objs = fromMaybe patt mergedPattern where
    -- The predicates that have been added to s'
    deltaAdd = cArgForm $ s' \\ s
    -- The predicates that have been removed from s
    deltaRem = cArgForm $ s \\ s'

    -- The predicates that were in both s and s'
    omegaS = cArgForm $ s' `Set.intersection` s

    -- The predicates that were not present in s, and could have been positive
    -- preconditions for failed actions
    notS   = cArgForm $ allPreds \\ s

    allShapes = shapes (Set.fromList pSpecs)
    allPreds = Set.unions $ Set.toList
             $ Set.map (Set.fromList . predsFromShape) allShapes

    cArgForm :: State -> Set (Predicate CArg)
    cArgForm = Set.map (fmap (objMap !))

    shapes :: Set (Predicate a) -> Set (String, Int)
    shapes set = Set.map (\p -> (predName p, predArity p)) set

    predsFromShape (n, a) = [Predicate n os | os <- (replicateM a objs)]

    -- The predicates whose shape was in deltaS, but were not themselves in s'
    posFailed = cArgForm $ Set.unions $ Set.toList
              $ Set.map ((\\ s') . Set.fromList . predsFromShape) (shapes deltaAdd)

    negFailed = undefined

    objMap = Map.fromList $ zip objs [1 ..]

    posSuccPatterns = Set.toList $ Set.map posSuccPatternFor deltaAdd
    negSuccPatterns = Set.toList $ Set.map negSuccPatternFor deltaRem
    posFailPatterns = Set.toList $ Set.map posFailPatternFor posFailed
    negFailPatterns = Set.toList $ Set.map negFailPatternFor negFailed

    posEffKnl p = PDDL.EffKnowledge $ PDDL.Hyp
                        (Set.singleton p, Set.empty)
                        (omegaS,          Set.empty)

    negEffKnl p = PDDL.EffKnowledge $ PDDL.Hyp
                        (Set.empty, Set.singleton p)
                        (Set.empty, notS)

    posCands p = Set.singleton (notS \\ Set.singleton p, cArgForm s)
    negCands p = Set.singleton (cArgForm s, notS \\ Set.singleton p)

    posPreKnl p = PDDL.PreKnowledge (PDDL.Hyp TSet.empty TSet.empty)
                                    (posCands p)

    negPreKnl p = PDDL.PreKnowledge (PDDL.Hyp TSet.empty TSet.empty)
                                    (negCands p)

    posSuccPatternFor p = Pattern (posEffKnl p) emptyPreKnl
    negSuccPatternFor p = Pattern (negEffKnl p) emptyPreKnl

    posFailPatternFor p = Pattern (posEffKnl p) (posPreKnl p)
    negFailPatternFor p = Pattern (negEffKnl p) (negPreKnl p)

    mergedPattern = merges (  posSuccPatterns ++ negSuccPatterns
                           ++ posFailPatterns ++ negFailPatterns
                           )

update :: DomainKnowledge -> Transition -> Pattern
update dk (s, a, s') = undefined
