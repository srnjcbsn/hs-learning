module Learning.OptPrecondLearn where

import           Learning.Induction
import           PDDL.Logic
import           PDDL

import           Data.Map           (Map, (!))
import qualified Data.Map           as Map
import           Data.Set           (Set, (\\))
import qualified Data.Set           as Set
import qualified Data.TupleSet      as TSet

type CNF = Set (Set FluentPredicate, Set FluentPredicate)
-- | (unknowns, knowns)
type Knowledge = (Set FluentPredicate, Set FluentPredicate)

-- | (Positives, Negatives, Candidates)
type PreKnowledge = (Knowledge, Knowledge, CNF)

type PreDomainHypothesis = Map Name PreKnowledge

initialPreKnowledge :: [Name] -> [PredicateSpec] -> [Name] -> PreKnowledge
initialPreKnowledge consts allPs paras =
    let paras' = map Ref paras ++ map Const consts
        unkns = Set.unions $ map (allFluents paras') allPs
        kn = (unkns, Set.empty)
    in (kn, kn, Set.empty)

initialPreDomainHyp :: Domain -> PreDomainHypothesis
initialPreDomainHyp dom =
    let mapper aSpec = ( asName aSpec
                       , initialPreKnowledge (dmConstants dom)
                                             (dmPredicates dom)
                                             (asParas aSpec)
                       )

    in Map.fromList $ fmap mapper (dmActionsSpecs dom)

constructPrecondSchema :: PreKnowledge -> ActionSpec -> ActionSpec
constructPrecondSchema ((_, posKnown), (_, negKnown), cnf) aSpec =
    aSpec { asPrecond = Con predList }
    where negPredList (poss,negs) =  Set.toList (Set.map Predicate negs)
                                  ++ Set.toList (Set.map (Neg . Predicate) poss)
          orList = Neg . Con . negPredList
          predList =  Set.toList (Set.map Predicate posKnown)
                   ++ Set.toList (Set.map (Neg . Predicate) negKnown)
                   ++ Set.toList (Set.map orList cnf)

domainFromPrecondHypothesis :: Domain -> PreDomainHypothesis -> Domain
domainFromPrecondHypothesis dom dHyp =
 dom { dmActionsSpecs = map (\as -> constructPrecondSchema (dHyp ! asName as) as)
                            $ dmActionsSpecs dom
     }


withoutSuperSetsOf :: CNF -> (Set FluentPredicate, Set FluentPredicate) -> CNF
withoutSuperSetsOf cnfSets subset =
    Set.filter (not . TSet.isSubSetOf subset) cnfSets


addToCandiates :: CNF -> (Set FluentPredicate, Set FluentPredicate) -> CNF
addToCandiates curCands cand = newCands
  where
    -- Remove all which are more broad requirements
    tmpCands = curCands `withoutSuperSetsOf` cand
    newCands = Set.insert cand tmpCands


isSingleton :: (Set FluentPredicate, Set FluentPredicate) -> Bool
isSingleton = (== 1) . TSet.size

removeSetsWithKnowns :: CNF -> (Set FluentPredicate, Set FluentPredicate) -> CNF
removeSetsWithKnowns cnfs kns = Set.filter (not . TSet.isSubSetOf kns) cnfs

extractKnowns :: CNF -> (Set FluentPredicate, Set FluentPredicate, CNF)
extractKnowns cnfs = (posKns, negKns, newCnfs')
  where
    singletons = Set.filter isSingleton cnfs
    -- remove empty sets and singletons
    newCnfs = Set.filter ((> 1) . TSet.size ) cnfs
    kns@(posKns, negKns) = Set.foldl TSet.union TSet.empty singletons
    -- If a set of candidates suddenly contain knowns
    -- then the other candidates become worthless (as they could all be false)
    newCnfs' = removeSetsWithKnowns newCnfs kns

updatePreDomainHyp :: Domain
                   -> PreDomainHypothesis
                   -> Transition
                   -> PreDomainHypothesis
updatePreDomainHyp dom hyp transition =
    let (_, (name, _), _) = transition
        pkn = hyp ! name
    in Map.insert name (updatePrecHypothesis dom pkn transition) hyp

updatePrecHypothesis :: Domain -> PreKnowledge -> Transition -> PreKnowledge
updatePrecHypothesis domain (posKnowledge, negKnowledge, cnfs) (s, action, s') =
    let aSpecParas = asParas $ findActionSpec domain action
        unground' :: GroundedPredicate -> Set FluentPredicate
        unground' = ungroundNExpand aSpecParas (aArgs action)

        (posUnkns, posKns) = posKnowledge
        (negUnkns, negKns) = negKnowledge
        rel unkns = Set.unions
                  $ Set.toList
                  $ Set.map unground'
                  $ ground domain action unkns `Set.intersection` s

        posRelevant = rel posUnkns
        negRelevant = rel negUnkns
    in case s' of
         Nothing  -> ( (posUnkns \\ posKns', posKns')
                     , (negUnkns \\ negKns', negKns')
                     , cnfs'
                     )
            where -- All predicates that are not in the state are candidates
                  -- for being positive preconditions
                  posCands = posUnkns \\ posRelevant

                  -- All predicates that are in the state are candidates
                  -- for being negative preconditions
                  negCands = negUnkns `Set.intersection` negRelevant
                  cands = (posCands, negCands)
                  (posKns', negKns', cnfs')
                    | isSingleton cands =
                        ( Set.union posKns posCands
                        , Set.union negKns negCands
                        , removeSetsWithKnowns cnfs cands
                        )
                    | otherwise = (posKns, negKns, addToCandiates cnfs cands)

         Just _ -> ( (posUnkns' \\ extractPosKns, Set.union extractPosKns posKns)
                   , (negUnkns' \\ extractNegKns, Set.union extractNegKns negKns)
                   , cnfs'')
            where -- All preds not in the state cant be a positive precond
                  posUnkns' = posUnkns `Set.intersection` posRelevant
                  -- All preds in the state cant be a negative precond
                  negUnkns' = negUnkns \\ negRelevant
                  -- all proved to be false can't be candidates
                  cnfs' = Set.map (TSet.intersection (posUnkns', negUnkns')) cnfs
                  (extractPosKns, extractNegKns, cnfs'')  = extractKnowns cnfs'
