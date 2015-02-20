module PDDL.Logic ( negateF
                  , conjunction
                  , isActionValid
                  , apply
                  , findActionSpec
                  , instantiateFormula
                  , instantiateAction
                  , ground
                  , applyAction
                  ) where

import qualified Data.List     as List
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe    (fromJust)
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Tuple    (swap)
import           PDDL

import qualified Data.TupleSet as Set2

-- flatten :: Formula -> Formula
-- flatten (Con ((Con f) : fs)) = map flatten f `conjunction` map flatten fs
-- flatten (Neg (Con fs))       = Con (map (Neg . flatten) fs)
-- flatten f                    = f

-- | Negate a 'Formula'. If the given 'Formula' is a conjunction, the contained
--   'Formula'e are negated recursively.
negateF :: Formula -> Formula
negateF (Con fs)        = Con $ map negateF fs
negateF p@(Predicate _) = Neg p
negateF (Neg f)         = f

-- | Forms a 'Conjunction' from two 'Formula'e.
conjunction :: Formula -> Formula -> Formula
conjunction (Con c1) (Con c2)       = Con (c1 ++ c2)
conjunction p@(Predicate _) (Con c) = Con (p : c)
conjunction (Con c) p@(Predicate _) = Con (p : c)
conjunction (Neg f1) f2 = Neg (conjunction f1 f2)
conjunction f1 (Neg f2) = Neg (conjunction f1 f2)
conjunction f1 f2 = Con [f1, f2]

-- | Finds the action spec of an action in a domain
findActionSpec :: Domain -> Action -> Maybe ActionSpec
findActionSpec domain (name, _) = actionSpec domain name


-- | Instantiates a formula into the actual positive and negative changes
insForm :: Map Argument Object -> Formula -> GroundedChanges
insForm m (Predicate p) = (Set.singleton (pName p, List.map (m Map.!) $ pArgs p), Set.empty)
insForm m (Neg f) = swap $ insForm m f
insForm m (Con fs) = List.foldl (\changes f -> Set2.union changes $ insForm m f ) (Set.empty,Set.empty) fs

-- | instantiates an Action into the actual precondions and the actual effect
insAct :: Map Argument Object -> ActionSpec -> Action -> GroundedAction
insAct m as act = ga
  where
    pairs = List.zip (List.map Ref $ asParas as) (aArgs act)
    paraMap = Map.fromList pairs
    fullMap = Map.union paraMap m
    ga = (insForm fullMap (asPrecond as), insForm fullMap (asEffect as))

-- | Checks if the preconditions of a grounded action are satisfied
isActionValid :: State -> GroundedAction -> Bool
isActionValid s ((posCond, negCond), _) =
  Set.isSubsetOf posCond s &&
  Set.null (Set.intersection negCond s)

-- | Applies the grounded actions to a state, if the action is not valid nothing is returned
applyAction :: State -> GroundedAction -> Maybe State
applyAction s act@(_,(posEff,negEff)) =
    if isActionValid s act then
      Just $ Set.union (Set.difference s negEff) posEff
    else Nothing

domainMap :: Domain -> Map Argument Object
domainMap domain = Map.fromList $ List.map (\n -> (Const n, n)) (dmConstants domain)

-- | Instantiates a formula into the actual positive and negative changes
instantiateFormula :: Domain
                   -> [Name]          -- ^ Parameters
                   -> [Name]          -- ^ Arguments
                   -> Formula
                   -> GroundedChanges
instantiateFormula domain paras args form = insForm fullMap form
    where pairs = List.zip (List.map Ref paras) args
          paraMap = Map.fromList pairs
          fullMap = Map.union paraMap $ domainMap domain

-- | Instantiates a formula into the actual positive and negative changes
instantiateAction :: Domain -> ActionSpec -> Action -> GroundedAction
instantiateAction domain as a =
  let mapDomain = domainMap domain
  in  insAct mapDomain as a

ground :: Domain
       -> Action
       -> Set FluentPredicate
       -> Set GroundedPredicate
ground domain (name, args) fluents =
    fst $ instantiateFormula domain paras args
        (Con $ List.map Predicate $ Set.toList fluents)
    where paras = asParas (fromJust $ actionSpec domain name)

-- | Takes an action, grounds it and then if the precondions are satisfied applies it to a state
apply :: Domain -> State -> Action -> Maybe State
apply domain state action =
    case findActionSpec domain action of
        Just aSpec -> applyAction state $ instantiateAction domain aSpec action
        Nothing    -> Nothing
