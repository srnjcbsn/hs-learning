module Learning.PDDL.EffectKnowledge  where

import           Learning.Induction
import qualified Learning.PDDL as Lrn
import           Logic.Formula
import           Planning
import           Planning.PDDL
import           Planning.PDDL.Logic

import           Data.Function           (on)
import           Data.List               (deleteBy)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Set                (Set, (\\))
import qualified Data.Set                as Set
import           Data.Typeable

-- -- | (unknown, known)
-- type EffectKnowledge = (Set FluentPredicate, Set FluentPredicate)
--
-- -- | (Positive, negative)
-- type EffectHypothesis = (EffectKnowledge, EffectKnowledge)

type EffectKnowledge = Lrn.EffKnowledge Argument


-- | Updates the effect hypothesis based on the transition
updateEffectHyp :: PDDLDomain
                -> EffectKnowledge
                -> Lrn.Transition
                -> EffectKnowledge
-- if the action application was unsuccessful, we cannot learn anything
updateEffectHyp domain (Lrn.EffKnowledge hyp) (s, action, s')
    | s == s' = Lrn.EffKnowledge hyp
    | otherwise =
        let aSpec = findActionSpec domain action
            aSpecParas = asParas aSpec
            -- (posEffects, negEffects) = ak
            -- (posUnkEff, posKnEff) = posEffects
            -- (negUnkEff, negKnEff) = negEffects

            unground' :: GroundedPredicate -> Set FluentPredicate
            unground' = ungroundNExpand aSpecParas (aArgs action)

            unions :: Ord a => Set (Set a) -> Set a
            unions = Set.unions . Set.toList

            gAdd :: Set GroundedPredicate
            gAdd = addList aSpec action domain
            kAdd = s' \\ s
            uAdd = s' `Set.intersection` s `Set.intersection` gAdd

            kDel = s \\ s'

            kAddUg = Set.map unground' kAdd
            uAddUg = Set.map unground' uAdd

            kDelUg = Set.map unground' kDel

            alphaAdd = unions kAddUg `Set.intersection` Lrn.posKnown hyp
            betaAdd  = unions uAddUg `Set.intersection` Lrn.posUnknown hyp

            alphaDel = unions kDelUg `Set.intersection` Lrn.negUnknown hyp

            tmpUnkAdd = alphaAdd `Set.union` betaAdd

            (posUamb, posAmb) =
                Set.foldl (extractUnambiguous tmpUnkAdd) (Set.empty, Set.empty) kAddUg
            (negUamb, negAmb) =
                Set.foldl (extractUnambiguous alphaDel) (Set.empty, Set.empty) kDelUg

            posUnkEff' = betaAdd `Set.union` posAmb
            posKnEff' = Lrn.posKnown hyp `Set.union` posUamb

            negUnkEff' = negAmb
            negKnEff' = Lrn.negKnown hyp `Set.union` negUamb

            posEff' = (posUnkEff', posKnEff')
            negEff' = (negUnkEff', negKnEff')

            ak' = (posEff', negEff')

            hyp' = Lrn.Hyp (posKnEff', negKnEff') (posUnkEff', negUnkEff')

            in Lrn.EffKnowledge hyp'

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


addList :: ActionSpec -> Action -> PDDLDomain -> Set GroundedPredicate
addList aSpec action _ = fst $ snd $ instantiateAction aSpec action
