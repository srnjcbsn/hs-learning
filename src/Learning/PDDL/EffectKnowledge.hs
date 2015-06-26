module Learning.PDDL.EffectKnowledge  where

import           Learning.Induction
import qualified Learning.PDDL.NonConditionalTypes as NCT

import           Planning
import           Planning.PDDL

import           Data.Set                          (Set, (\\))
import qualified Data.Set                          as Set


-- | Updates the effect hypothesis based on the transition
updateEffectKnl :: PDDLDomain
                -> AllPossibleObjects
                -> NCT.EffKnowledge
                -> Transition
                -> NCT.EffKnowledge
-- if the action application was unsuccessful, we cannot learn anything
updateEffectKnl domain allobjs (NCT.EffKnowledge knl) (s, action, s')
    | s == s' = NCT.EffKnowledge knl
    | otherwise = NCT.EffKnowledge knl' where
        aSpec = findActionSpec domain action
        aSpecParas = asParas aSpec

        unground' :: GroundedPredicate -> Set FluentPredicate
        unground' = unground aSpecParas (aArgs action)

        unions :: Ord a => Set (Set a) -> Set a
        unions = Set.unions . Set.toList

        gAdd :: Set GroundedPredicate
        gAdd = addList  aSpec action domain allobjs s
        kAdd = s' \\ s
        uAdd = s' `Set.intersection` s `Set.intersection` gAdd

        kDel = s \\ s'

        kAddUg = Set.map unground' kAdd
        uAddUg = Set.map unground' uAdd

        kDelUg = Set.map unground' kDel

        alphaAdd = unions kAddUg `Set.intersection`
                    (NCT.posKnown knl `Set.union` NCT.posUnknown knl)
        betaAdd  = unions uAddUg `Set.intersection` NCT.posUnknown knl

        alphaDel = unions kDelUg `Set.intersection` NCT.negUnknown knl

        tmpUnkAdd = alphaAdd `Set.union` betaAdd

        (posUamb, posAmb) =
            Set.foldl (extractUnambiguous tmpUnkAdd) (Set.empty, Set.empty) kAddUg
        (negUamb, negAmb) =
            Set.foldl (extractUnambiguous alphaDel) (Set.empty, Set.empty) kDelUg

        posUnkEff' = betaAdd `Set.union` posAmb
        posKnEff' = NCT.posKnown knl `Set.union` posUamb

        negUnkEff' = negAmb
        negKnEff' = NCT.negKnown knl `Set.union` negUamb

        knl' = NCT.Knowledge (posKnEff', negKnEff') (posUnkEff', negUnkEff')


-- | Takes a set of uknowns, a pair of unambiguous/ambiguous to add to,
--   and a set of fluents which is suspected to be unambiguous.
--   once determined they/it will be added to its respective set
extractUnambiguous :: Set FluentPredicate
                   -> (Set FluentPredicate, Set FluentPredicate)
                   -> Set FluentPredicate
                   -> (Set FluentPredicate, Set FluentPredicate)
extractUnambiguous unk (uAmb,amb) test =
    case unambiguate unk test of
        Left uap  -> (Set.insert uap uAmb, amb)
        Right aps -> (uAmb, Set.union amb aps)

addList :: ActionSpec -> Action -> PDDLDomain -> AllPossibleObjects -> State -> Set GroundedPredicate
addList aSpec action _ allobjs s = fst $ instantiateAction allobjs aSpec s action
