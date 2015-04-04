module Learning.OptEffectLearn  where

import           Learning.Induction
import qualified Learning.SchemaLearning as Lrn
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

type EffDomainHypothesis = Map Name EffectKnowledge

newtype OptEffHypothesis = OptEffHypothesis EffDomainHypothesis
                           deriving (Show, Eq, Typeable)

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
addList aSpec action _ = fst $ snd $ instantiateAction aSpec action

-- | Sets the effects of the provided ActionSpec to that of the effect hypothesis
constructEffectSchema :: EffectKnowledge -> ActionSpec -> ActionSpec
constructEffectSchema (Lrn.EffKnowledge hyp) action  =
    let addL = Set.map Pred $ (Lrn.posUnknown hyp) `Set.union` (Lrn.posKnown hyp)
        delL = Set.map (Neg . Pred) (Lrn.negKnown hyp)
        effect = Con $ Set.toList $ addL `Set.union` delL
    in action { asEffect = effect }

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

-- updateEffectHyp _ ak (_, _, Nothing) = ak
-- updateEffectHyp domain ak (oldState, action, Just newState) =
--     let aSpec = findActionSpec domain action
--         aSpecParas = asParas aSpec
--         (posEffects, negEffects) = ak
--         (posUnkEff, posKnEff) = posEffects
--         (negUnkEff, negKnEff) = negEffects
--
--         unground' :: GroundedPredicate -> Set FluentPredicate
--         unground' = ungroundNExpand aSpecParas (aArgs action)
--
--         unions :: Ord a => Set (Set a) -> Set a
--         unions = Set.unions . Set.toList
--
--         gAdd :: Set GroundedPredicate
--         gAdd = addList aSpec action domain
--         kAdd = newState \\ oldState
--         uAdd = newState `Set.intersection` oldState `Set.intersection` gAdd
--
--         kDel = oldState \\ newState
--
--         kAddUg = Set.map unground' kAdd
--         uAddUg = Set.map unground' uAdd
--
--         kDelUg = Set.map unground' kDel
--
--         alphaAdd = unions kAddUg `Set.intersection` posUnkEff
--         betaAdd  = unions uAddUg `Set.intersection` posUnkEff
--
--         alphaDel = unions kDelUg `Set.intersection` negUnkEff
--
--         tmpUnkAdd = alphaAdd `Set.union` betaAdd
--
--         (posUamb, posAmb) =
--             Set.foldl (extractUnambiguous tmpUnkAdd) (Set.empty, Set.empty) kAddUg
--         (negUamb, negAmb) =
--             Set.foldl (extractUnambiguous alphaDel) (Set.empty, Set.empty) kDelUg
--
--         posUnkEff' = betaAdd `Set.union` posAmb
--         posKnEff' = posKnEff `Set.union` posUamb
--
--         negUnkEff' = negAmb
--         negKnEff' = negKnEff `Set.union` negUamb
--
--         posEff' = (posUnkEff', posKnEff')
--         negEff' = (negUnkEff', negKnEff')
--
--         ak' = (posEff', negEff')
--
--         in ak'

effectHypothesis :: EffDomainHypothesis -> Name -> EffectKnowledge
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
domainFromKnowledge :: PDDLDomain -> EffDomainHypothesis -> PDDLDomain
domainFromKnowledge dom dHyp =
    dom { dmActionsSpecs = map (\as -> constructEffectSchema (effectHypothesis dHyp (asName as)) as)
                         $ dmActionsSpecs dom
        }

initialEffKnowledge :: [Name] -> [PredicateSpec] -> [Name] -> EffectKnowledge
initialEffKnowledge consts allPs paras = Lrn.EffKnowledge hyp where
    paras' = map Ref paras ++ map Const consts
    unkns  = Set.unions $ map (allFluents paras') allPs
    hyp    = Lrn.Hyp (Set.empty, Set.empty) (unkns, unkns)

-- | Constructs a hypothesis based on what is known about the domain
initialHypothesis :: PDDLDomain -> EffDomainHypothesis
initialHypothesis dom = Map.fromList $ fmap mapper (dmActionsSpecs dom) where
    mapper aSpec = ( asName aSpec
                   , initialEffKnowledge (dmConstants dom)
                                         (dmPredicates dom)
                                         (asParas aSpec)
                   )

-- | Updates the effects hypothesis based on the transition
updateEffectHypHelper :: PDDLDomain
                      -> EffDomainHypothesis
                      -> Lrn.Transition
                      -> EffDomainHypothesis
updateEffectHypHelper dom dHyp transition = Map.insert (aName action) aHyp' dHyp
    where (_, action, _) = transition
          aHyp = effectHypothesis dHyp (aName action)
          aHyp' = updateEffectHyp dom aHyp transition

-- | If application of the action in the provided transition would yield the
--   same new state as the environment, the old action hypothesis is returned
--   unchanged. Otherwise, the hypothesis is updated with the new information.
updateDomainHyp :: PDDLDomain
                -> EffDomainHypothesis
                -> Lrn.Transition
                -> EffDomainHypothesis
updateDomainHyp dom dHyp transition
    | planState == Just newState = dHyp
    | otherwise = updateEffectHypHelper dom dHyp transition
    where (oldState, action, newState) = transition
          planState = apply dom oldState action
