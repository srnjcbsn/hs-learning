module Learning.PDDL.PreconditionKnowledge where

import           Learning.Induction
import           Planning
import           Planning.PDDL
import           Planning.PDDL.Logic

import           Data.Set                (Set, (\\))
import qualified Data.Set                as Set
import qualified Data.TupleSet           as TSet
import qualified Learning.PDDL as Lrn

type CNF = Lrn.Cands Argument
type PreKnowledge = Lrn.PreKnowledge Argument



update :: PDDLDomain -> PreKnowledge -> Transition -> PreKnowledge
update domain (Lrn.PreKnowledge hyp cnfs) (s, action, s')
    | s == s'   =
        let -- All predicates that are not in the state are candidates
            -- for being positive preconditions
            posCands = posUnkns \\ posRelevant

            -- All predicates that are in the state are candidates
            -- for being negative preconditions
            negCands = negUnkns `Set.intersection` negRelevant
            cands = (posCands, negCands)

            (posKns', negKns', cnfs')
                | isSingleton cands = ( Set.union posKns posCands
                                      , Set.union negKns negCands
                                      , removeSetsWithKnowns cnfs cands
                                      )
                | otherwise =  (posKns, negKns, addToCandiates cnfs cands)

            hyp' = Lrn.Hyp (posKns', negKns')
                         (posUnkns \\ posKns', negUnkns \\ negKns')

        in Lrn.PreKnowledge hyp' cnfs'

    | otherwise =
        let -- All preds not in the state cant be a positive precond
            posUnkns' = posUnkns `Set.intersection` posRelevant
            -- All preds in the state cant be a negative precond
            negUnkns' =  negUnkns \\ negRelevant
            -- all proved to be false can't be candidates
            cnfs' = Set.map (TSet.intersection (posUnkns', negUnkns')) cnfs
            (extractPosKns, extractNegKns, cnfs'')  = extractKnowns cnfs'

            hyp' = Lrn.Hyp ( Set.union extractPosKns posKns
                           , Set.union extractNegKns negKns
                           )
                           (posUnkns' \\ extractPosKns, negUnkns \\ extractNegKns)

        in Lrn.PreKnowledge hyp' cnfs''

    where unground' :: GroundedPredicate -> Set FluentPredicate
          unground' = ungroundNExpand aSpecParas (aArgs action)
          aSpecParas = asParas $ findActionSpec domain action

          posUnkns = (fst . Lrn.unknowns) hyp
          posKns   = (fst . Lrn.knowns) hyp
          negUnkns = (snd . Lrn.unknowns) hyp
          negKns   = (snd . Lrn.knowns) hyp

          rel unkns = Set.unions
                    $ Set.toList
                    $ Set.map unground'
                    $ ground domain action unkns `Set.intersection` s

          posRelevant = rel posUnkns
          negRelevant = rel negUnkns

addToCandiates :: CNF -> (Set FluentPredicate, Set FluentPredicate) -> CNF
addToCandiates curCands cand = newCands
  where
    -- Remove all which are more broad requirements
    tmpCands = curCands `withoutSuperSetsOf` cand
    newCands = Set.insert cand tmpCands

withoutSuperSetsOf :: CNF -> (Set FluentPredicate, Set FluentPredicate) -> CNF
withoutSuperSetsOf cnfSets subset =
    Set.filter (not . TSet.isSubSetOf subset) cnfSets

isSingleton :: (Set FluentPredicate, Set FluentPredicate) -> Bool
isSingleton = (== 1) . TSet.size

removeSetsWithKnowns :: CNF -> (Set FluentPredicate, Set FluentPredicate) -> CNF
removeSetsWithKnowns cnfs kns
    | TSet.size kns > 0 = Set.filter (not . TSet.doesOverlap kns) cnfs
    | otherwise = cnfs

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
