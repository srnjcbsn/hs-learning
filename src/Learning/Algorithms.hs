module Learning.Algorithms where

import           Control.Monad      (replicateM)
import           Data.Function      (on)
import           Data.List          (deleteBy, unionBy)
import           Data.Map           (Map, (!))
import qualified Data.Map           as Map
import           Data.Maybe         (catMaybes, fromJust)
import           Data.Set           (Set, (\\))
import qualified Data.Set           as Set
import           Debug.Trace

import           Learning.Deduction
import           PDDL.Logic
import           PDDL.Type

type EffectKnowledge = (Set FluentPredicate, Set FluentPredicate)
type ActionKnowledge = (EffectKnowledge, EffectKnowledge)

type ActionHypothesis = (ActionSpec, ActionKnowledge)

type DomainKnowledge = Map Name ActionKnowledge
type DomainHypothesis = (Domain, DomainKnowledge)

type Transition = (State, State, Action)

domain :: DomainHypothesis -> Domain
domain = fst

knowledge :: DomainHypothesis -> DomainKnowledge
knowledge = snd

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

updateActionHyp :: Domain -> ActionHypothesis -> Transition -> ActionHypothesis
updateActionHyp domain (aSpec, ak) transition =
    let (oldState, newState, action) = transition
        (posEffects, negEffects) = ak
        (posUnkEff, posKnEff) = posEffects
        (negUnkEff, negKnEff) = negEffects

        unground' :: GroundedPredicate -> Set FluentPredicate
        unground' gp = Set.fromList $ (flip expandFluents $ (fst gp))
                     $ unground (asParas aSpec) (aArgs action) (snd gp)

        unions :: Ord a => Set (Set a) -> Set a
        unions = Set.unions . Set.toList

        gAdd :: Set GroundedPredicate
        gAdd = fst . snd $ instantiateAction domain aSpec action
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
            Set.foldl (folder tmpUnkAdd) (Set.empty, Set.empty) kAddUg
        (negUamb, negAmb) =
            Set.foldl (folder alphaDel) (Set.empty, Set.empty) kDelUg

        posUnkEff' = betaAdd `Set.union` posAmb
        posKnEff' = posKnEff `Set.union` posUamb

        negUnkEff' = negAmb
        negKnEff' = negKnEff `Set.union` negUamb

        posEff' = (posUnkEff', posKnEff')
        negEff' = (negUnkEff', negKnEff')

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
            as : deleteBy ((==) `on` asName) as (dmActionsSpecs dom)
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
          paras as  = map Ref (asParas as) ++ map Const (dmConstants dom)
          eff as    = Set.unions
                    $ map (allFluents $ paras as)
                    $ dmPredicates dom
          dom' = domainFromKnowledge dom knowledge

updateActionHypHelper :: DomainHypothesis
                      -> Transition
                      -> DomainHypothesis
updateActionHypHelper (dom, know) transition =
    (dom', Map.insert (fst action) (snd aHyp') know)
    where (oldState, newState, action) = transition
          aHyp = actionHypothesis (dom, know) action
          aHyp' = updateActionHyp dom aHyp transition
          dom' = updateDomain dom (fst aHyp')

-- | If application of the action in the provided transition would yield the
--   same new state as the environment, the old actin hypothesis is returned
--   unchanged. Otherwise, the hypothesis is updated with the new information.
updateDomainHyp :: DomainHypothesis
                -> Transition
                -> DomainHypothesis
updateDomainHyp (dom, know) transition
    | planState == newState = (dom, know)
    | otherwise = updateActionHypHelper (dom, know) transition
    --(dom', Map.insert (fst action) (snd aHyp') know)
    where planState = fromJust $ apply dom oldState action
          (oldState, newState, action) = transition
        --   aHyp = actionHypothesis (dom, know) action
        --   aHyp' = updateActionHyp dom aHyp transition
        --   dom' = updateDomain dom (fst aHyp')
