module Learning.OptPrecondLearn where

import PDDL.Type
import Learning.Algorithms
import PDDL.Logic
import Learning.Induction

import           Data.Set           (Set, (\\))
import qualified Data.Set           as Set
import qualified Data.TupleSet      as Set2

type CNF = Set (Set FluentPredicate, Set FluentPredicate)
-- | (unknowns, knowns)
type Knowledge = (Set FluentPredicate, Set FluentPredicate)

-- | (Positives, Negatives, Candidates)
type PreKnowledge = (Knowledge, Knowledge, CNF)

type Transition' = (State, Action, Maybe State)

constructSchema :: PreKnowledge -> ActionSpec -> ActionSpec
constructSchema = undefined


withoutSuperSetsOf :: CNF -> (Set FluentPredicate, Set FluentPredicate) -> CNF
withoutSuperSetsOf cnfSets subset = Set.filter (not . Set2.isSubSetOf subset) cnfSets


addToCandiates :: CNF -> (Set FluentPredicate, Set FluentPredicate) -> CNF
addToCandiates curCands cand = newCands
  where
    tmpCands = curCands `withoutSuperSetsOf` cand -- Remove all which are more broad requirements
    newCands = Set.insert cand tmpCands


isSingleton :: (Set FluentPredicate, Set FluentPredicate) -> Bool
isSingleton = (== 1) . Set2.size

removeSetsWithKnowns :: CNF -> (Set FluentPredicate, Set FluentPredicate) -> CNF
removeSetsWithKnowns cnfs kns = Set.filter (not . Set2.isSubSetOf kns) cnfs

extractKnowns :: CNF -> (Set FluentPredicate, Set FluentPredicate, CNF)
extractKnowns cnfs = (posKns, negKns, newCnfs')
  where
    singletons = Set.filter (isSingleton) cnfs
    newCnfs = Set.filter ((> 1) . Set2.size ) cnfs -- remove empty sets and singletons
    kns@(posKns, negKns) = Set.foldl Set2.union (Set.empty, Set.empty) singletons
    newCnfs' = removeSetsWithKnowns newCnfs kns -- If a set of candidates suddenly contain knowns
                                                -- then the other candidates become worthless (as they could all be false)



updatePrecHypothesis :: [Name] -> PreKnowledge -> Transition' -> PreKnowledge
updatePrecHypothesis aSpecParas pk@(posKnowledge, negKnowledge, cnfs) (s, action, s') =
    let unground' :: GroundedPredicate -> Set FluentPredicate
        unground' gp = Set.fromList $ (flip expandFluents $ (fst gp))
                 $ unground aSpecParas (aArgs action) (snd gp)

        (posUnkns, posKns) = posKnowledge
        (negUnkns, negKns) = negKnowledge
        rel unkns = Set.unions
                  $ Set.toList
                  $ Set.map unground'
                  $ ground unkns `Set.intersection` s

        posRelevant = rel posUnkns
        negRelevant = rel negUnkns
    in case s' of
         Nothing  -> ((posUnkns,posKns'), (negUnkns, negKns'), cnfs')
            where posCands = posUnkns \\ posRelevant -- All preds which are not in the state must be positive candidates
                  negCands = negUnkns `Set.intersection` negRelevant -- All preds which are in the state must be negative candidates
                  cands = (posCands,negCands)
                  (posKns',negKns',cnfs')
                    | isSingleton cands = (Set.union posKns posCands, Set.union negKns negCands, removeSetsWithKnowns cnfs cands )
                    | otherwise = (posKns, negKns, addToCandiates cnfs cands)

         Just _ -> ((posUnkns',Set.union extractPosKns posKns ),(negUnkns', Set.union extractNegKns negKns),cnfs'')
            where posUnkns' = posUnkns `Set.intersection` posRelevant -- All preds not in the state cant be a positive precond
                  negUnkns' = negUnkns \\ negRelevant                 -- All preds in the state cant be a negative precond
                  cnfs' = Set.map (Set2.intersection (posUnkns', negUnkns')) cnfs -- all proved to be false can't be candidates
                  (extractPosKns, extractNegKns, cnfs'')  = extractKnowns cnfs'
