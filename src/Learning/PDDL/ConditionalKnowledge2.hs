module Learning.PDDL.ConditionalKnowledge2 where

import           Data.TupleSet
import           Data.Typeable
import           Environment
import qualified Learning                            as Lrn
import qualified Learning.Induction                  as Ind
import qualified Learning.PDDL                       as PDDL
import qualified Learning.PDDL.EffectKnowledge       as Eff
import qualified Learning.PDDL.PreconditionKnowledge as Pre
import           Logic.Formula                       hiding (Formula, Neg)
import qualified Logic.Formula                       as LF
import           Planning
import           Planning.PDDL                       hiding (Formula, Neg)

import           Control.Monad                       (liftM, replicateM,
                                                      sequence)
import           Data.Function                       (on)
import           Data.List                           (elemIndices, intersectBy)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe
import           Data.Set                            (Set, (\\))
import qualified Data.Set                            as Set
import           Data.Tree
import           Data.TupleSet                       (TupleSet)
import qualified Data.TupleSet                       as TSet

-- type DomainKnowledge = Map Name Pattern

data Binding = Bound CArg
             | Free
             deriving (Eq, Ord, Show)

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

data Literal a = Pos a
               | Not a
               deriving (Eq, Ord, Show)

getLit :: Literal a -> a
getLit (Pos a) = a
getLit (Not a) = a

getLitName :: LitPred a -> Literal Name
getLitName (Pos p) = Pos $ predName p
getLitName (Not p) = Not $ predName p

type LitPred a = Literal (Predicate a)
type Cands a = Set (Set (LitPred a))

type CondPred = LitPred Binding

data Pattern = Pattern
    { patEffect    :: LitPred Object
    , patPreconds  :: State
    , patPredSpecs :: [PredicateSpec]
    , patObjects   :: [Object]
    } deriving (Ord, Eq, Show)

data ConditionalKnowledge = ConditionalKnowledge
    { ckEffectArgs   :: [Binding]
    , ckPreconds :: Set CondPred
    , ckMaxArg   :: CArg
    , ckCands    :: Cands Binding
    , ckKnown    :: Set CondPred
    } deriving (Ord, Eq, Show)

newtype ActionKnowledge =
    ActionKnowledge (Map (Literal Name) ConditionalKnowledge)

newtype DomainKnowledge = DomainKnowledge (Map Name ActionKnowledge)

nextMax :: ConditionalKnowledge -> (CArg, ConditionalKnowledge)
nextMax ck = (m', ck { ckMaxArg = m' }) where m' = succ (ckMaxArg ck)

universeState :: [PredicateSpec]
              -> [Object]
              -> State
universeState specs objs = Set.fromList allPreds where
  args = flip replicateM objs
  toPreds (Predicate n a) = map (Predicate n) (args $ length a)
  allPreds = concatMap toPreds specs

initialConditionalKnowledge :: Pattern -> ConditionalKnowledge
initialConditionalKnowledge pat =
  ConditionalKnowledge
    { ckEffectArgs   = fromJust $ objsToArgs $ (predArgs . getLit) eff
    , ckPreconds = negPreLit `Set.union` posPreLit
    , ckCands    = Set.empty
    , ckKnown    = Set.empty
    , ckMaxArg   = length objs
    } where
  s = patPreconds pat
  eff = patEffect pat
  objs = patObjects pat

  -- Possible positive preconditions
  posPre = setVarForm s
  -- Possible negative preconditions
  negPre = setVarForm $ universeState (patPredSpecs pat) objs \\ s

  negPreLit = Set.map Not negPre
  posPreLit = Set.map Pos posPre

  setVarForm objForm = Set.fromList $ mapMaybe varForm $ Set.toList objForm
  varForm (Predicate n os) = liftM (Predicate n) $ objsToArgs os
  objsToArgs = mapM (`Map.lookup` objMap)
  objMap :: Map Object Binding
  objMap = Map.fromList $ zip objs (map Bound [1 .. ])

updateConditionalKnowledge :: ConditionalKnowledge
                           -> Pattern
                           -> ConditionalKnowledge
updateConditionalKnowledge = onSuccess

updateActionKnowledge :: ActionKnowledge -> Pattern -> ActionKnowledge
updateActionKnowledge (ActionKnowledge ak) pat =
    let alteration Nothing   = Just $ initialConditionalKnowledge pat
        alteration (Just ck) = Just $ updateConditionalKnowledge ck pat
        ak' = Map.alter (alteration) (getLitName . patEffect $ pat) ak
    in  ActionKnowledge ak'

type Connection = (Int, Literal Name)

connections :: Binding -> Set CondPred -> [(Connection, CondPred)]
connections b cps = concatMap mapper $ Set.toList cps where
    mapper p = zip (matches p) (repeat p)
    matches p =
        [ (i, getLitName p) | i <- elemIndices b $ (predArgs . getLit) p]

change :: Int -> a -> [a] -> [a]
change n el ls = hl ++ (el : (tail ll)) where
    (hl, ll) = splitAt n ls

containsConnection :: Pattern -> Connection -> Bool
containsConnection pat conn = undefined

removeBindings :: Binding -> [(Int, CondPred)] -> Set CondPred -> Set CondPred
removeBindings newB bs cps = foldl modify cps bs where
    modify :: Set CondPred -> (Int, CondPred) -> Set CondPred
    modify cps' (n, p) = Set.insert (newPred n p) cps' -- $ Set.delete p cps'

    newPred :: Int -> CondPred -> CondPred
    newPred n (Pos p)  = Pos $ Predicate ((predName) p)
                            (change n newB (predArgs p))

merge :: Binding
      -> Binding
      -> Set CondPred
      -> Set CondPred
      -> Set CondPred
merge b1 b2 cps1 cps2 = undefined where
    c1 = connections b1 cps1
    c2 = connections b2 cps2
    generalizations = intersectBy ((==) `on` fst) c1 c2

connected :: CondPred -> Set CondPred -> Set CondPred
connected focus set = undefined

onSuccess :: ConditionalKnowledge -> Pattern -> ConditionalKnowledge
onSuccess ck pat2 = undefined where
    -- pat1 = ckPattern ck

onFailure :: ConditionalKnowledge -> Pattern -> ConditionalKnowledge
onFailure = undefined

failed :: [PredicateSpec]
       -> [Object]
       -> ActionKnowledge
       -> Transition
       -> [Pattern]
failed = undefined



constructPattern :: [PredicateSpec]
                 -> [Object]
                 -> State
                 -> LitPred Object
                 -> Pattern
constructPattern specs objs predUnks ep =
  Pattern { patEffect = ep
          , patPreconds = predUnks
          , patPredSpecs = specs
          , patObjects   = objs
          }

toPatterns :: [PredicateSpec] -> [Object] -> Transition -> [Pattern]
toPatterns specs objs (s, _ , s') = deltaAddPatns ++ deltaRemPatns where
  -- The predicates that have been added to s'
  deltaAdd = Set.toList $ s' \\ s
  -- The predicates that have been removed from s
  deltaRem = Set.toList $ s \\ s'
  -- -- Possible positive preconditions
  -- posPre = varForm $ s
  -- -- Possible negative preconditions
  -- negPre = varForm $ universeState specs objs \\ s

  cPat = constructPattern specs objs s
  --allPreconditions = (Set.map Pos posPre) `Set.union` (Set.map Neg negPre)
  deltaAddPatns = map (cPat . Pos) deltaAdd
  deltaRemPatns = map (cPat . Not) deltaRem

  --toPattern :: (a -> Literal a) -> Predicate CArg -> (Literal Name, Pattern)
  --toPattern et p = constructPattern (et p) allPreconditions

  -- varForm objForm = Set.fromList $ mapMaybe f $ Set.toList objForm
  -- f (Predicate n os) = liftM (Predicate n) $ mapM (`Map.lookup` objMap) os
  -- objMap :: Map Object Binding
  -- objMap = Map.fromList $ zip objs (map Bound [1 ..])

updateKnowledge :: PDDLDomain
                -> PDDLProblem
                -> DomainKnowledge
                -> Transition
                -> DomainKnowledge
updateKnowledge = undefined
