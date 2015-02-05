module Learning.Algorithms where

import Data.List (unionBy, find, permutations)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromJust)

import PDDL.Logic
import PDDL.Type
import Learning.Deduction

data Knowledge a = Known a
                 | Unknown a

-- come up with better name
type Combination = (Name, [Set Argument])

predicates :: Combination -> [FluentPredicate]
predicates (name, args) = expandFluents args name

combineDeductions :: Ord a => [ [ Set a ] ] -> [ [ Set a ] ] -> [ [ Set a ] ]
combineDeductions = undefined

extract :: Knowledge a -> a
extract (Known a)   = a
extract (Unknown a) = a

known :: Knowledge a -> Maybe a
known (Known a)   = Just a
known (Unknown _) = Nothing

unknown :: Knowledge a -> Maybe a
unknown (Known _)   = Nothing
unknown (Unknown a) = Just a

type ActionKnowledge = ([Knowledge Combination], [Knowledge Combination])
type ActionHypothesis = (ActionSpec, ActionKnowledge)

type DomainKnowledge = Map Name ActionKnowledge
type DomainHypothesis = (Domain, DomainKnowledge)

type Transition = (State, State, Action)

addList :: ActionSpec -> Action -> Domain -> Set GroundedPredicate
addList = undefined

isAmbiguous :: Knowledge Combination -> Bool
isAmbiguous (Known _) = False
isAmbiguous (Unknown (_, args)) = not $ all (((==) 1) . Set.size) args

constructSchema :: ActionHypothesis -> ActionSpec
constructSchema (action, (posEff, negEff)) =
    let posEffects = concatMap ( map Predicate . predicates . extract) posEff
        negEffects = concatMap (map (Neg . Predicate) . predicates) (mapMaybe known negEff)
        effect     = Con $ posEffects ++ negEffects

    in ActionSpec { asName    = asName action
                  , asParas   = asParas action
                  , asPrecond = asPrecond action
                  , asEffect  = effect
                  }

updateActionHyp :: Domain -> ActionHypothesis -> Transition -> ActionHypothesis
updateActionHyp domain (aSpec, ak) transition =
    let (oldState, newState, action) = transition
        (posEffects, negEffects) = ak

        unground' :: GroundedPredicate -> [Knowledge Combination]
        unground' = unground (asParas aSpec) (aArgs action)
                  . snd

        -- predicates to be removed from the add list
        remAdd = Set.map unground' $ addList aSpec action domain \\ newState
        -- predicates that are now known to be in the add list
        addAdd = Set.map unground' $ newState \\ oldState
        -- predicates to be added to the delete list
        deltaDel = Set.map unground' $ oldState \\ newState

        negEffects' = negEffects ++ (Set.toList deltaDel)

        -- (flip expandFluents $ fst action)
        in undefined

        -- del = Set.toList $ oldState \\ newState
        -- add = Set.toList $ newState \\ oldState

        -- rewrite this
        -- unground' :: [GroundedPredicate] -> [Knowledge FluentPredicate]
        -- unground' = map $ unground (asParas aSpec) (snd action) (dmConstants domain)
        -- unground' = map Known
        --           . Set.toList
        --           . Set.unions
        --           . (map $ (unground (asParas aSpec) (snd action) (dmConstants domain)))

        -- shouldUpdateKnowledge (Known a) (Unknown b) = a == b
        -- shouldUpdateKnowledge _ _ = False

        -- posEffects' =
        --
        -- posEffects' = unionBy shouldUpdateKnowledge (unground' add) posEffects
        -- newKnowledge = (posEffects', negEffects ++ unground' del)

    -- in undefined -- (constructSchema (aSpec, newKnowledge), newKnowledge)

updateDomainHyp :: DomainHypothesis -> Transition -> DomainHypothesis
updateDomainHyp hyp transition =
    let (_, _, action) = transition
        aHyp = actionHypothesis hyp action
        aHyp' = updateActionHyp (fst hyp) aHyp transition
    in  (fst hyp, Map.insert (fst action) (snd aHyp') (snd hyp))

-- constructAllSchemas :: [ActionSpec] -> DomainKnowledge -> [ActionSpec]
-- constructAllSchemas aSpecs knowledge =
--     let knowledgeForAction a = knowledge ! (asName a)
--         act a = constructSchema (knowledgeForAction a) a
--     in  map act $ aSpecs

actionHypothesis :: DomainHypothesis -> Action -> ActionHypothesis
actionHypothesis (domain, dmKnowledge) action =
    (fromJust $ actionSpec domain (fst action), dmKnowledge ! (fst action))

-- reqrite to using this
-- analyzePlan :: DomainHypothesis
--             -> [Action]
--             -> Transition
--             -> (State -> Action -> State)
--             -> (DomainHypothesis, Maybe State)

analyzePlan :: Domain
            -> [Action]
            -> (State -> Action -> Maybe State)
            -> State
            -> DomainHypothesis
            -> (DomainHypothesis, Maybe State)

analyzePlan _ [] _ _ dHyp = (dHyp, Nothing)
analyzePlan domain (action : rest) evalAction oldState dHyp =
    if planState == actualState
    then analyzePlan domain rest evalAction actualState dHyp
    else (updateDomainHyp dHyp transition, Just actualState)
        where planState    = fromJust $ apply domain oldState action -- verify that this can never be Nothing
              actualState  = fromJust $ evalAction oldState action -- same as above
              transition   = (oldState, actualState, action)

effectLearn :: Planner -> [Problem] -> Domain -> ()
effectLearn planner episodes domain = undefined
