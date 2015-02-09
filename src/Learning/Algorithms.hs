module Learning.Algorithms where

import Data.List (unionBy, deleteBy)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromJust)
import Control.Monad (replicateM)

import PDDL.Logic
import PDDL.Type
import Learning.Deduction

type EffectKnowledge = (Set FluentPredicate, Set FluentPredicate)
type ActionKnowledge = (EffectKnowledge, EffectKnowledge)

type ActionHypothesis = (ActionSpec, ActionKnowledge)

type DomainKnowledge = Map Name ActionKnowledge
type DomainHypothesis = (Domain, DomainKnowledge)

type Transition = (State, State, Action)

ambiguous :: Set (Set FluentPredicate) -> Set FluentPredicate
ambiguous = undefined

unAmbiguous :: Set (Set FluentPredicate) -> Set FluentPredicate
unAmbiguous = undefined

folder :: Set FluentPredicate
       -> (Set FluentPredicate, Set FluentPredicate)
       -> Set FluentPredicate
       -> (Set FluentPredicate, Set FluentPredicate)
folder unk (uAmb,amb) test =
    case unambiguate unk test of
        Left uap  -> (Set.insert uap uAmb, amb)
        Right aps -> (uAmb, Set.union amb aps)


addList :: ActionSpec -> Action -> Domain -> Set GroundedPredicate
addList aSpec action dom = fst $ snd $ instantiateAction dom aSpec action

constructSchema :: ActionSpec -> ActionKnowledge -> ActionSpec
constructSchema action ak =
    let ((unkPosEff, knPosEff), (_, knNegEff)) = ak
        addL = Set.map Predicate $ unkPosEff `Set.union` knPosEff
        delL = Set.map (Neg . Predicate) knNegEff
        effect = Con $ Set.toList $ addL `Set.union` delL
    in action { asEffect = effect }
    -- in ActionSpec { asName    = asName action
    --               , asParas   = asParas action
    --               , asPrecond = asPrecond action
    --               , asEffect  = effect
    --               }

updateActionHyp :: Domain -> ActionHypothesis -> Transition -> ActionHypothesis
updateActionHyp domain (aSpec, ak) transition =
    let (oldState, newState, action) = transition
        (posEffects, negEffects) = ak
        (posUnkEff, posKnEff) = posEffects
        (negUnkEff, negKnEff) = negEffects

        unground' :: GroundedPredicate -> Set FluentPredicate
        unground' gp = Set.fromList $ (flip expandFluents $ (fst gp))
                     $ unground (asParas aSpec) (aArgs action) (snd gp)

        unchanged = oldState `Set.intersection` newState
        -- predicates that are now known to be in the add list
        addAdd = newState \\ oldState

         -- predicates to be added to the delete list
        addDel = oldState \\ newState

        uUnchanged = Set.unions
                   $ Set.toList
                   $ Set.map unground' unchanged

        uAddAdd = Set.map unground' addAdd
        uAddDel = Set.map unground' addDel

        (posUamb, posAmb) =
            Set.foldl (folder posUnkEff) (Set.empty, Set.empty) uAddAdd

        (negUamb, negAmb) =
            Set.foldl (folder negUnkEff) (Set.empty, Set.empty) uAddDel

        negKn' = negUamb `Set.union` negKnEff
        posKn' = posUamb `Set.union` posKnEff

        uUnchanged' = (uUnchanged \\ negKn') \\ posKn'

        posUnk' = posAmb `Set.union` uUnchanged'
        negUnk' = negAmb `Set.union` uUnchanged'

        posEff' = (posUnk', posKn')
        negEff' = (negUnk', negKn')
        ak' = (posEff', negEff')

        in (constructSchema aSpec ak', ak')

actionHypothesis :: DomainHypothesis -> Action -> ActionHypothesis
actionHypothesis (domain, dmKnowledge) (an, _) =
    (fromJust $ actionSpec domain an, dmKnowledge ! an)

allFluents :: [Argument] -> PredicateSpec -> Set FluentPredicate
allFluents paras (name, args) = Set.fromList
                              $ map ((,) name) $ replicateM (length args) paras

-- update the domain by inserting the given action specification
-- in place of the existing one with the same name
updateDomain :: Domain -> ActionSpec -> Domain
updateDomain dom as =
    dom { dmActionsSpecs =
            as : deleteBy (\a1 a2 -> asName a1 == asName a2) as (dmActionsSpecs dom)
        }

domainFromKnowledge :: Domain -> DomainKnowledge -> Domain
domainFromKnowledge dom kn =
    dom { dmActionsSpecs = map (\as -> constructSchema as $ kn ! asName as)
                         $ dmActionsSpecs dom
        }

initialHypothesis :: Domain -> DomainHypothesis
initialHypothesis dom = (dom', knowledge)
    where knowledge = foldl ins Map.empty $ dmActionsSpecs dom
          ins m as  = Map.insert (asName as) (aKn as) m
          aKn as    = ((eff as, Set.empty), (eff as, Set.empty))
          paras as  = (map Ref $ asParas as) ++ (map Const $ dmConstants dom)
          eff as    = Set.unions
                    $ map (allFluents $ paras as)
                    $ dmPredicates dom
          dom' = domainFromKnowledge dom knowledge


-- | If application of the action in the provided transition would yield the
--   same new state as the environment, the old actin hypothesis is returned
--   unchanged. Otherwise, the hypothesis is updated with the new information.
updateDomainHyp :: DomainHypothesis
                -> Transition
                -> DomainHypothesis
updateDomainHyp (dom, know) transition
    | planState == newState = (dom, know)
    | otherwise = (dom', Map.insert (fst action) (snd aHyp') know)
    where planState = fromJust $ apply dom oldState action
          (oldState, newState, action) = transition
          aHyp = actionHypothesis (dom, know) action
          aHyp' = updateActionHyp dom aHyp transition
          dom' = updateDomain dom (fst aHyp')
