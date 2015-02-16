module Learning.OptEffectLearn where

import           Data.Function      (on)
import           Data.List          (deleteBy, unionBy)
import           Data.Map           (Map, (!))
import qualified Data.Map           as Map
import           Data.Maybe         (catMaybes, fromJust)
import           Data.Set           (Set, (\\))
import qualified Data.Set           as Set
import           Debug.Trace

import           Learning.Induction
import           PDDL.Logic
import           PDDL.Type

-- | (unknown, known)
type EffectKnowledge = (Set FluentPredicate, Set FluentPredicate)

-- | (Positive, negative)
type EffectHypothesis = (EffectKnowledge, EffectKnowledge)

type DomainHypothesis = Map Name EffectHypothesis



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


addList :: ActionSpec -> Action -> Domain -> Set GroundedPredicate
addList aSpec action dom = fst $ snd $ instantiateAction dom aSpec action

-- | Sets the effects of the provided ActionSpec to that of the effect hypothesis
constructEffectSchema :: EffectHypothesis -> ActionSpec -> ActionSpec
constructEffectSchema ak action  =
    let ((unkPosEff, knPosEff), (_, knNegEff)) = ak
        addL = Set.map Predicate $ unkPosEff `Set.union` knPosEff
        delL = Set.map (Neg . Predicate) knNegEff
        effect = Con $ Set.toList $ addL `Set.union` delL
    in action { asEffect = effect }

-- | Updates the effect hypothesis based on the transition
updateEffectHyp :: Domain -> EffectHypothesis -> Transition -> EffectHypothesis
updateEffectHyp domain ak transition =
    let (oldState, action, mNewState) = transition
        newState = fromJust mNewState
        aSpec = fromJust $ findActionSpec domain action
        aSpecParas = asParas aSpec
        (posEffects, negEffects) = ak
        (posUnkEff, posKnEff) = posEffects
        (negUnkEff, negKnEff) = negEffects

        unground' :: GroundedPredicate -> Set FluentPredicate
        unground' = ungroundNExpand aSpecParas (aArgs action)

        unions :: Ord a => Set (Set a) -> Set a
        unions = Set.unions . Set.toList

        gAdd :: Set GroundedPredicate
        gAdd = fst $
            instantiateFormula domain aSpecParas (aArgs action) (asEffect aSpec)
        kAdd = newState \\ oldState
        uAdd = newState `Set.intersection` oldState `Set.intersection` gAdd

        kDel = oldState \\ newState

        kAddUg = Set.map unground' kAdd
        uAddUg = Set.map unground' uAdd

        kDelUg = Set.map unground' kDel

        alphaAdd = unions kAddUg `Set.intersection` posUnkEff
        betaAdd  = unions uAddUg `Set.intersection` posUnkEff

        alphaDel = unions kDelUg `Set.intersection` negUnkEff

        tmpUnkAdd = alphaAdd `Set.union` betaAdd

        (posUamb, posAmb) =
            Set.foldl (extractUnambiguous tmpUnkAdd) (Set.empty, Set.empty) kAddUg
        (negUamb, negAmb) =
            Set.foldl (extractUnambiguous alphaDel) (Set.empty, Set.empty) kDelUg

        posUnkEff' = betaAdd `Set.union` posAmb
        posKnEff' = posKnEff `Set.union` posUamb

        negUnkEff' = negAmb
        negKnEff' = negKnEff `Set.union` negUamb

        posEff' = (posUnkEff', posKnEff')
        negEff' = (negUnkEff', negKnEff')

        ak' = (posEff', negEff')

        in ak'

effectHypothesis :: DomainHypothesis -> Action -> EffectHypothesis
effectHypothesis dmKnowledge (actName, _) = dmKnowledge ! actName


-- update the domain by inserting the given action specification
-- in place of the existing one with the same name
updateDomain :: Domain -> ActionSpec -> Domain
updateDomain dom as =
    dom { dmActionsSpecs =
            as : deleteBy ((==) `on` asName) as (dmActionsSpecs dom)
        }
-- | Changes all the domain's action specs effect based on the hypothesis
domainFromKnowledge :: Domain -> DomainHypothesis -> Domain
domainFromKnowledge dom dHyp =
    dom { dmActionsSpecs = map (\as -> constructEffectSchema (dHyp ! asName as) as)
                         $ dmActionsSpecs dom
        }

-- | Constructs a hypothesis based on what is known about the domain
initialHypothesis :: Domain -> DomainHypothesis
initialHypothesis dom = dHyp
    where dHyp = foldl ins Map.empty $ dmActionsSpecs dom
          ins m as  = Map.insert (asName as) (aKn as) m
          aKn as    = ((eff as, Set.empty), (eff as, Set.empty))
          paras as  = map Ref (asParas as) ++ map Const (dmConstants dom)
          eff as    = Set.unions
                    $ map (allFluents $ paras as)
                    $ dmPredicates dom


-- | Updates the effects hypothesis based on the transition
updateEffectHypHelper :: Domain
                      -> DomainHypothesis
                      -> Transition
                      -> DomainHypothesis
updateEffectHypHelper dom dHyp transition = Map.insert (aName action) aHyp' dHyp
    where (oldState, action, newState) = transition
          aHyp = effectHypothesis dHyp action
          aHyp' = updateEffectHyp dom aHyp transition

-- | If application of the action in the provided transition would yield the
--   same new state as the environment, the old actin hypothesis is returned
--   unchanged. Otherwise, the hypothesis is updated with the new information.
updateDomainHyp :: Domain
                -> DomainHypothesis
                -> Transition
                -> DomainHypothesis
updateDomainHyp dom dHyp transition
    | Just planState == newState = dHyp
    | otherwise = updateEffectHypHelper dom dHyp transition
    where (oldState, action, newState) = transition
          planState = fromJust $ apply dom oldState action
