module Learning.Algorithms where

import Data.List (unionBy, find, permutations)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromJust)

import PDDL.Logic
import PDDL.Type
import Learning.Deduction

type EffectKnowledge = (Set FluentPredicate, Set FluentPredicate)
type ActionKnowledge = (EffectKnowledge, EffectKnowledge)

type ActionHypothesis = (ActionSpec, ActionKnowledge)

type DomainKnowledge = Map Name ActionKnowledge
type DomainHypothesis = (Domain, DomainKnowledge)

type Transition = (State, State, Action)

addList :: ActionSpec -> Action -> Domain -> Set GroundedPredicate
addList aSpec action dom = undefined

constructSchema :: ActionSpec -> ActionKnowledge -> ActionSpec
constructSchema action ak =
    let ((unkPosEff, knPosEff), (_, knNegEff)) = ak
        addL = Set.map Predicate $ unkPosEff `Set.union` knPosEff
        delL = Set.map (Neg . Predicate) knNegEff
        effect = Con $ Set.toList $ addL `Set.union` delL
    in ActionSpec { asName    = asName action
                  , asParas   = asParas action
                  , asPrecond = asPrecond action
                  , asEffect  = effect
                  }

updateActionHyp :: Domain -> ActionHypothesis -> Transition -> ActionHypothesis
updateActionHyp domain (aSpec, ak) transition =
    let (oldState, newState, action) = transition
        (posEffects, negEffects) = ak
        (posUnkEff, posKnEff) = posEffects
        (negUnkEff, negKnEff) = negEffects

        unground' :: GroundedPredicate -> Set FluentPredicate
        unground' gp = Set.fromList $ (flip expandFluents $ (fst gp))
                     $ unground (asParas aSpec) (aArgs action) (snd gp)

         -- predicates to be removed from the add list
        remAdd = addList aSpec action domain
               \\ newState `Set.union` oldState

        -- predicates that are now known to be in the add list
        addAdd = newState \\ oldState

        remDel = oldState `Set.intersection` newState
         -- predicates to be added to the delete list
        addDel = oldState \\ newState

        unkEff :: Set FluentPredicate
               -> Set GroundedPredicate
               -> Set FluentPredicate

        unkEff fps gps = reducePossibilities fps
                       $ Set.toList
                       $ Set.map unground' gps

        knEff :: Set FluentPredicate
              -> Set GroundedPredicate
              -> Set FluentPredicate

        knEff fps gps = Set.fromList
                      $ catMaybes
                      $ Set.toList
                      $ Set.map (unambiguate fps)
                      $ Set.map unground' gps

        posUnkEff' = unkEff posUnkEff remAdd
        negUnkEff' = unkEff negUnkEff remDel
        posKnEff' = knEff posUnkEff addAdd
        negKnEff' = knEff negUnkEff addDel

        posEff' = (posUnkEff', posKnEff `Set.union` posKnEff')
        negEff' = (negUnkEff', negKnEff `Set.union` negKnEff')
        ak' = (posEff', negEff')

        in (constructSchema aSpec ak', ak')

actionHypothesis :: DomainHypothesis -> Action -> ActionHypothesis
actionHypothesis (domain, dmKnowledge) action =
    (fromJust $ actionSpec domain (fst action), dmKnowledge ! (fst action))

-- | If application of the action in the provided transition would yield the
-- | same new state as the environment, the old actin hypothesis is returned
-- | unchanged. Otherwise, the hypothesis is updated with the new information.
updateDomainHyp :: DomainHypothesis
                -> Transition
                -> DomainHypothesis
updateDomainHyp hyp transition
    | planState == newState = hyp
    | otherwise = (fst hyp, Map.insert (fst action) (snd aHyp') (snd hyp))
    where planState = fromJust $ apply (fst hyp) oldState action
          (oldState, newState, action) = transition
          aHyp = actionHypothesis hyp action
          aHyp' = updateActionHyp (fst hyp) aHyp transition
