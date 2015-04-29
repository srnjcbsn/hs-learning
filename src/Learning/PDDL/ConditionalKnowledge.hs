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

import           Control.Monad                       (sequence, replicateM)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe
import           Data.Set                            (Set, (\\))
import qualified Data.Set                            as Set
import           Data.Tree
import           Data.TupleSet                       (TupleSet)
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

type Match = (CArg, CArg)

type Unification = Set (Predicate Match) -- Map Name [Set Match]

data MetaPattern = MetaPattern { mtPres :: (Unification, Unification)
                               , mtEffs :: (Unification, Unification)
                               }

type Contradiction = Int
type ContSet  = Set Contradiction

removeUnconnected :: MetaPattern -> MetaPattern
removeUnconnected = undefined

fromMetaPattern :: MetaPattern -> Pattern
fromMetaPattern = undefined

toContradiction :: Predicate Match -> Set Contradiction
toContradiction = undefined

contradicts :: Predicate Match -> Set Contradiction -> Bool
contradicts p conts = toContradiction p `Set.intersection` conts == Set.empty



unsLookup :: Ord k => String -> k -> Map k v -> v
unsLookup err k m =
  case Map.lookup k m of
     Just v -> v
     Nothing -> error $  "Tried to look up non-existing key "
                      ++ "in map. (" ++ err ++ ")."

predConns :: (Match -> CArg)
          -> Set CArg
          -> [Set Match]
          -> [Set Match]
predConns fn aSet mSets = sigs significants where
    reduced = map ((Set.intersection aSet) . (Set.map fn)) mSets
    significants = length (filter ((> 0) . Set.size) reduced)
    -- If no args are connected, return list of empty sets
    sigs 0 = map (const Set.empty) mSets
    -- If exactly one argument place is connected by a set of arguments,
    -- then all arguments are connected, except the unconnected ones in
    -- that particular argument place
    sigs 1 = zipWith combine reduced mSets
    -- If more than one argument place contained connected arguments, all
    -- arguments are connected
    sigs _ = mSets

    combine :: Set CArg -> Set Match -> Set Match
    combine args ms | Set.null args = ms
                    | otherwise     = Set.filter ((`Set.member` args) . fn) ms

connected :: Set CArg
          -> Set CArg
          -> (Match -> CArg)
          -> (Unification, Unification)
          -> Unification
connected frontier expl sel space = undefined
    -- | Set.null frontier = Map.empty
    -- | otherwise         = retval where
    -- (space1, space2) = space
    -- retval = Map.unionWith (zipWith Set.union) ret' conns
    -- ret'   = connected frontier' expl' sel (space1', space2')
    -- frontier' = Map.fold (\a b -> collectArgs a `Set.union` b) Set.empty conns
    -- expl' = expl \\ frontier
    -- mkConns sp = Map.map (predConns sel frontier) sp
    -- conns = Map.unionWith (zipWith Set.union) (mkConns space1) (mkConns space2)
    -- space1' = Map.mapWithKey removeConns space1
    -- space2' = Map.mapWithKey removeConns space2

    -- collectArgs :: [Set Match] -> Set CArg
    -- collectArgs mSets = foldl (\acc this -> (this \\ expl) `Set.union` acc) Set.empty
    --                   $ map (Set.map sel) mSets
    --
    -- removeConns :: Name -> [Set Match] -> [Set Match]
    -- removeConns k a = case Map.lookup k conns of
    --                        Just s  -> zipWith Set.difference a s
    --                        Nothing -> a

extractArguments :: Unification -> Set Match
extractArguments uMap = undefined
    --  Map.foldl extList Set.empty uMap where
    -- extList set ls = (foldl Set.union Set.empty ls) `Set.union` set

reachable :: MetaPattern
          -> MetaPattern
reachable (MetaPattern (posPre, negPre) (posEff, negEff)) = undefined
    -- let -- The initial frontier is all the args occurring in effects
    --     frontier = extractArguments posEff `Set.union` extractArguments negEff
    --
    --     front1 = Set.map fst frontier
    --     front2 = Set.map snd frontier
    --
    --     conns1 = connected front1 Set.empty fst (posPre, negPre)
    --     conns2 = connected front2 Set.empty snd (posPre, negPre)
    --
    --     mapIntersect = Map.intersectionWith (zipWith Set.intersection)
    --
    --     mergedConns = mapIntersect conns1 conns2
    --     posPre' = mapIntersect posPre mergedConns
    --     negPre' = mapIntersect negPre mergedConns
    --
    -- in  (MetaPattern (posPre', negPre') (posEff, negEff))

tupleMap :: (a -> b) -> (a, a) -> (b, b)
tupleMap f (t1, t2) = (f t1, f t2)

instantiatePattern :: (Pattern, Pattern) -> MetaPattern -> Pattern
instantiatePattern pats mp = undefined
    -- MetaPattern (mpPosPre, mpNegPre) (mpPosEff, mpNegEff) = mp
    --
    -- pk p = PDDL.pkHyp (ctPreconds p)
    -- ek p = PDDL.ekHyp (ctEffects p)
    --
    -- posPreKn   = tupleMap (PDDL.posKnown . pk) pats
    -- negPreKn   = tupleMap (PDDL.negKnown . pk) pats
    -- posPreUnkn = tupleMap (PDDL.posUnknown . pk) pats
    -- negPreUnkn = tupleMap (PDDL.posUnknown . pk) pats
    --
    -- posEffKn   = tupleMap (PDDL.posKnown . ek) pats
    -- negEffKn   = tupleMap (PDDL.negKnown . ek) pats
    -- posEffUnkn = tupleMap (PDDL.posUnknown . ek) pats
    -- negEffUnkn = tupleMap (PDDL.negUnknown . ek) pats
    --
    -- posEffKnUg = unground mpPosEff posEffKn
    -- negEffKnUg = unground mpNegEff negEffKn
    -- posEffUnknUg = unground mpPosEff posEffUnkn
    -- negEffUnknUg = unground mpNegEff negEffUnkn
    --
    -- newU (u1, u2) = TSet.intersection u1 u2
    -- newK (u1, u2) (k1, k2) = TSet.union (TSet.intersection k1 k2)
    --                        ( TSet.union (TSet.intersection k1 u2)
    --                                     (TSet.intersection k2 u1)
    --                        )
    --
    -- newEffKn = newK posEffUnknUg posEffUnknUg
    -- newEff
    --
    -- newPre = PDDL.Hyp { PDDL.knowns = newK preKnl1 preKnl2
    --                   , PDDL.unknowns = newU preKnl1 preKnl2
    --                   }
    -- newEff = PDDL.Hyp { PDDL.knowns = newK effKnl1 effKnl2
    --                   , PDDL.unknowns = newU effKnl1 effKnl2
    --                   }
    -- newPre' = PDDL.PreKnowledge newPre Set.empty
    -- newEff' = PDDL.EffKnowledge newEff

unground :: Unification
         -> TupleSet (Predicate CArg)
         -> TupleSet (Predicate Match)
unground uni (ps1, ps2) = undefined
  -- let selectMatches slctor p =
   --      do argsUni <- Map.lookup (predName p) uni
   --         let remover s e = Set.filter ((== e) . slctor) s
   --             argsUni' = zipWith remover argsUni (predArgs p)
   --             argsUni'' = map Set.toList argsUni'
   --         return $ map (Predicate (predName p)) $ sequence argsUni''
   --
   --    ug slctor ps = Set.fromList
   --                 $ concatMap id
   --                 $ mapMaybe (selectMatches slctor)
   --                 $ Set.toList ps
   -- in (ug fst ps1, ug snd ps2)

matchPredicates :: Unification
                -> Predicate CArg
                -> [[CArg]]
                -> Unification
matchPredicates uni p argLs  = undefined
  -- let zipMatches = zipWith Set.union
   --    zipper :: [CArg] -> [Set Match] -> [CArg] -> [Set Match]
   --    zipper args1 m args2 =
   --        let zped = map Set.singleton $ zip args1 args2
   --         in zipMatches zped m
   --    initArgs = replicate (length $ predArgs p) Set.empty
   --    newMatch = foldl (zipper $ predArgs p) initArgs argLs
   --    f Nothing = Just newMatch
   --    f (Just oldMatch) =  Just (zipMatches oldMatch newMatch)
   --    uni' = Map.alter f (predName p) uni
   -- in uni'

group :: (Ord a, Ord b)
      => Set (Predicate a)
      -> Set (Predicate b)
      -> [(Predicate a,[[b]])]
group ps1 ps2 =
    let matched ps p  = (p, map predArgs $ Set.toList $ Set.filter (matcher p) ps )
        matcher p = (== predName p) . predName
     in Set.toList $ Set.map (matched ps2) ps1

mapping :: Unification
        -> Set (Predicate CArg)
        -> Set (Predicate CArg)
        -> Unification
mapping cArgMap knl1 knl2 =
  let knlgroup = group knl1 knl2
      f m (p,argLs) = matchPredicates m p argLs
   in foldl f cArgMap knlgroup

initMapping :: Set (Predicate CArg)
            -> Set (Predicate CArg)
            -> Unification
initMapping = undefined -- mapping Map.empty

patternMapping :: Pattern -> Pattern -> MetaPattern
patternMapping p1 p2 =
  let ek p = PDDL.ekHyp (ctEffects p)
      pk p = PDDL.pkHyp (ctPreconds p)

      uKnl knl = TSet.union (PDDL.knowns knl) (PDDL.unknowns knl)

      pkP1 = pk p1
      pkP2 = pk p2
      ekP1 = ek p1
      ekP2 = ek p2

      (ppos1,pneg1) = uKnl pkP1
      (ppos2,pneg2) = uKnl pkP2

      (epos1,eneg1) = uKnl ekP1
      (epos2,eneg2) = uKnl ekP2

      ppUni = initMapping ppos1 ppos2
      npUni = initMapping pneg1 pneg2
      peUni = initMapping epos1 epos2
      neUni = initMapping eneg1 eneg2

   in MetaPattern { mtPres = (ppUni, npUni) , mtEffs = (peUni,neUni) }

unify :: Pattern -> Pattern -> (Pattern, Pattern)
unify p1 p2 = undefined
  -- let m = patternMapping p1 p2
  --  in unground (p1, p2) m

-- Only works as long as effects doesn't contain more of each effect IE.
-- p(x,y) p(y,z) <-- Not allowed
-- p(x,y) f(y) <-- Allowed
merge :: Pattern -> Pattern -> Pattern
merge p1 p2 =
  let (p1', p2') = unify p1 p2
      pk p = PDDL.pkHyp (ctPreconds p)
      ek p = PDDL.ekHyp (ctEffects p)

      uKnl knl = (PDDL.unknowns knl, PDDL.knowns knl)
      preKnl1 = uKnl (pk p1')
      effKnl1 = uKnl (ek p1')

      preKnl2 = uKnl (pk p2')
      effKnl2 = uKnl (ek p2')

      newU (u1, _) (u2, _) = TSet.intersection u1 u2
      newK (u1, k1) (u2, k2) = TSet.union (TSet.intersection k1 k2)
                             ( TSet.union (TSet.intersection k1 u2)
                                          (TSet.intersection k2 u1)
                             )
      newPre = PDDL.Hyp { PDDL.knowns = newK preKnl1 preKnl2
                        , PDDL.unknowns = newU preKnl1 preKnl2
                        }
      newEff = PDDL.Hyp { PDDL.knowns = newK effKnl1 effKnl2
                        , PDDL.unknowns = newU effKnl1 effKnl2
                        }
      newPre' = PDDL.PreKnowledge newPre Set.empty
      newEff' = PDDL.EffKnowledge newEff


   in Pattern { ctPreconds = newPre', ctEffects = newEff'}

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
    cArgForm = Set.map (fmap (\k -> unsLookup "fromTransition" k objMap))

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
