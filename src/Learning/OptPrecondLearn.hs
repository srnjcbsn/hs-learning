module Learning.OptPrecondLearn where

import PDDL.Type
import Learning.Algorithms
import PDDL.Logic
import Learning.Deduction

import           Data.Set           (Set, (\\))
import qualified Data.Set           as Set

type CNF = Set (Set FluentPredicate, Set FluentPredicate)
-- | (unknowns, knowns)
type Knowledge = (Set FluentPredicate, Set FluentPredicate)
type PreKnowledge = (Knowledge, Knowledge, CNF)

type Transition' = (State, Action, Maybe State)

constructSchema :: PreKnowledge -> ActionSpec -> ActionSpec
constructSchema = undefined

reduceCnfs :: PreKnowledge
           -> (Set FluentPredicate, Set FluentPredicate)
           -> PreKnowledge
reduceCnfs = undefined

updatePrecHypothesis :: ActionSpec -> PreKnowledge -> Transition' -> PreKnowledge
updatePrecHypothesis aSpec pk@(posKnowledge, negKnowledge, cnfs) (s, action, s') =
    let unground' :: GroundedPredicate -> Set FluentPredicate
        unground' gp = Set.fromList $ (flip expandFluents $ (fst gp))
                 $ unground (asParas aSpec) (aArgs action) (snd gp)

        (posUnkns, posKns) = posKnowledge
        (negUnkns, negKns) = negKnowledge
        rel unkns = Set.unions
                  $ Set.toList
                  $ Set.map unground'
                  $ ground unkns `Set.intersection` s

        posRelevant = rel posUnkns
        negRelevant = rel negUnkns
    in case s' of
         Nothing  -> reduceCnfs pk (posCandidates, negCandidates)
            where posCandidates = posUnkns \\ posRelevant
                  negCandidates = negUnkns `Set.intersection` negRelevant
                  cnfs' = Set.insert (posCandidates, negCandidates) cnfs
         Just s'' -> undefined
            where posUnkns' = posUnkns `Set.intersection` posRelevant
                  negUnkns' = negUnkns \\ negRelevant
