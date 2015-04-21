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

import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Set                            (Set, (\\))
import qualified Data.Set                            as Set
import           Data.Tree
import           Data.TupleSet

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
    { ctEffects :: PDDL.EffKnowledge CArg
    , ctPreconds :: PDDL.PreKnowledge CArg
    }

type CArgMap = (Map (CArg,CArg) CArg, CArg)

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

update :: Pattern -> Transition -> Pattern
update = undefined
