module Learning.OptPrecondLearn where

import           Data.Typeable
import           Learning.Induction
import           Logic.Formula
import           Planning.PDDL
import           Planning.PDDL.Logic


import           Data.Map                (Map, (!))
import qualified Data.Map                as Map
import           Data.Set                (Set, (\\))
import qualified Data.Set                as Set
import qualified Data.TupleSet           as TSet
import qualified Learning.SchemaLearning as Lrn

type CNF = Lrn.Cands Argument
type PreKnowledge = Lrn.PreKnowledge Argument

type PreDomainHypothesis = Map Name PreKnowledge

newtype OptPreHypothesis = OptPreHypothesis PreDomainHypothesis
                           deriving (Show, Eq, Typeable)

instance Lrn.DomainHypothesis OptPreHypothesis PDDLDomain PDDLProblem ActionSpec
    where
      update (OptPreHypothesis eff) dom trans  =
        OptPreHypothesis (updatePreDomainHyp dom eff trans)
      adjustDomain (OptPreHypothesis eff) dom  = domainFromPrecondHypothesis dom eff
      fromDomain dom = OptPreHypothesis ( initialPreDomainHyp dom )

initialPreKnowledge :: [Name] -> [PredicateSpec] -> [Name] -> PreKnowledge
initialPreKnowledge consts allPs paras = Lrn.PreKnowledge hyp Set.empty where
    paras' = map Ref paras ++ map Const consts
    unkns = Set.unions $ map (allFluents paras') allPs
    hyp = Lrn.Hyp (Set.empty, Set.empty) (unkns, unkns)

initialPreDomainHyp :: PDDLDomain -> PreDomainHypothesis
initialPreDomainHyp dom =
    let mapper aSpec = ( asName aSpec
                       , initialPreKnowledge (dmConstants dom)
                                             (dmPredicates dom)
                                             (asParas aSpec)
                       )

    in Map.fromList $ fmap mapper (dmActionsSpecs dom)

constructPrecondFormula :: PreKnowledge -> Formula Argument
-- constructPrecondFormula ((_, posKnown), (_, negKnown), cnf) =
constructPrecondFormula (Lrn.PreKnowledge hyp cnf) =
    Con predList
    where negPredList (poss,negs) =  Set.toList (Set.map Pred negs)
                                  ++ Set.toList (Set.map (Neg . Pred) poss)
          orList = Neg . Con . negPredList
          predList =  Set.toList (Set.map Pred (Lrn.posKnown hyp))
                   ++ Set.toList (Set.map (Neg . Pred) (Lrn.negKnown hyp))
                   ++ Set.toList (Set.map orList cnf)

constructPrecondSchema :: PreKnowledge -> ActionSpec -> ActionSpec
constructPrecondSchema preKnow aSpec =
    --trace ("precondForm (" ++ asName aSpec ++ "): " ++ ppShow preKnow)
    aSpec { asPrecond = constructPrecondFormula preKnow }

domainFromPrecondHypothesis :: PDDLDomain -> PreDomainHypothesis -> PDDLDomain
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

updatePreDomainHyp :: PDDLDomain
                   -> PreDomainHypothesis
                   -> Lrn.Transition
                   -> PreDomainHypothesis
updatePreDomainHyp dom hyp transition =
    let (_, (name, _), _) = transition
        pkn = hyp ! name
    in Map.insert name (updatePrecHypothesis dom pkn transition) hyp

updatePrecHypothesis :: PDDLDomain -> PreKnowledge -> Lrn.Transition -> PreKnowledge
updatePrecHypothesis domain (Lrn.PreKnowledge hyp cnfs) (s, action, s')
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
