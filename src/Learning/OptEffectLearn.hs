module Learning.OptEffectLearn (OptEffHypothesis) where

import           Data.Function       (on)
import           Data.List           (deleteBy, unionBy)
import           Data.Map            (Map, (!))
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes, fromJust)
import           Data.Set            (Set, (\\))
import qualified Data.Set            as Set
import           Debug.Trace
import Data.Typeable

import           Text.Show.Pretty (ppShow)
import           Learning.Induction
import           Logic.Formula
import           Planning.PDDL
import           Planning.PDDL.Logic
import qualified Learning as Lrn
-- | (unknown, known)
type EffectKnowledge = (Set FluentPredicate, Set FluentPredicate)

-- | (Positive, negative)
type EffectHypothesis = (EffectKnowledge, EffectKnowledge)

type DomainHypothesis = Map Name EffectHypothesis

newtype OptEffHypothesis = OptEffHypothesis DomainHypothesis deriving (Show, Eq, Typeable)

instance Lrn.DomainHypothesis OptEffHypothesis PDDLDomain PDDLProblem ActionSpec where
      update (OptEffHypothesis eff) dom trans  =
        OptEffHypothesis (updateDomainHyp dom eff trans)
      adjustDomain (OptEffHypothesis eff) dom  = domainFromKnowledge dom eff
      fromDomain dom = OptEffHypothesis ( initialHypothesis dom )
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
addList aSpec action dom = fst $ snd $ instantiateAction aSpec action

-- | Sets the effects of the provided ActionSpec to that of the effect hypothesis
constructEffectSchema :: EffectHypothesis -> ActionSpec -> ActionSpec
constructEffectSchema ak action  =
    let ((unkPosEff, knPosEff), (_, knNegEff)) = ak
        addL = Set.map Pred $ unkPosEff `Set.union` knPosEff
        delL = Set.map (Neg . Pred) knNegEff
        effect = Con $ Set.toList $ addL `Set.union` delL
    in action { asEffect = effect }

-- | Updates the effect hypothesis based on the transition
updateEffectHyp :: PDDLDomain -> EffectHypothesis -> Transition -> EffectHypothesis
-- if the action application was unsuccessful, we cannot learn anything
updateEffectHyp _ ak (_, _, Nothing) = ak
updateEffectHyp domain ak (oldState, action, Just newState) =
    let aSpec = findActionSpec domain action
        aSpecParas = asParas aSpec
        (posEffects, negEffects) = ak
        (posUnkEff, posKnEff) = posEffects
        (negUnkEff, negKnEff) = negEffects

        unground' :: GroundedPredicate -> Set FluentPredicate
        unground' = ungroundNExpand aSpecParas (aArgs action)

        unions :: Ord a => Set (Set a) -> Set a
        unions = Set.unions . Set.toList

        gAdd :: Set GroundedPredicate
        gAdd = addList aSpec action domain
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

effectHypothesis :: DomainHypothesis -> Name -> EffectHypothesis
effectHypothesis dmKnowledge actName =
    case Map.lookup actName dmKnowledge of
         Just ehyp -> ehyp
         Nothing -> error $ "Tried to retrieve effect hypothesis for action "
                            ++ actName ++ " which does not exist."


-- update the domain by inserting the given action specification
-- in place of the existing one with the same name
updateDomain :: PDDLDomain -> ActionSpec -> PDDLDomain
updateDomain dom as =
    dom { dmActionsSpecs =
            as : deleteBy ((==) `on` asName) as (dmActionsSpecs dom)
        }

-- | Changes all the domain's action specs effect based on the hypothesis
domainFromKnowledge :: PDDLDomain -> DomainHypothesis -> PDDLDomain
domainFromKnowledge dom dHyp =
    dom { dmActionsSpecs = map (\as -> constructEffectSchema (effectHypothesis dHyp (asName as)) as)
                         $ dmActionsSpecs dom
        }

-- | Constructs a hypothesis based on what is known about the domain
initialHypothesis :: PDDLDomain -> DomainHypothesis
initialHypothesis dom = dHyp
    where dHyp = foldl ins Map.empty $ dmActionsSpecs dom
          ins m as  = Map.insert (asName as) (aKn as) m
          aKn as    = ((eff as, Set.empty), (eff as, Set.empty))
          paras as  = map Ref (asParas as) ++ map Const (dmConstants dom)
          eff as    = Set.unions
                    $ map (allFluents $ paras as)
                    $ dmPredicates dom


-- | Updates the effects hypothesis based on the transition
updateEffectHypHelper :: PDDLDomain
                      -> DomainHypothesis
                      -> Transition
                      -> DomainHypothesis
updateEffectHypHelper dom dHyp transition = Map.insert (aName action) aHyp' dHyp
    where (_, action, _) = transition
          aHyp = effectHypothesis dHyp (aName action)
          aHyp' = updateEffectHyp dom aHyp transition

-- | If application of the action in the provided transition would yield the
--   same new state as the environment, the old action hypothesis is returned
--   unchanged. Otherwise, the hypothesis is updated with the new information.
updateDomainHyp :: PDDLDomain
                -> DomainHypothesis
                -> Transition
                -> DomainHypothesis
updateDomainHyp dom dHyp transition
    | planState == newState = dHyp
    | otherwise = updateEffectHypHelper dom dHyp transition
    where (oldState, action, newState) = transition
          planState = apply dom oldState action
