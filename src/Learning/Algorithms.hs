module Learning.Algorithms where

import Data.List (unionBy, find)
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

extract :: Knowledge a -> a
extract (Known a)   = a
extract (Unknown a) = a

known :: Knowledge a -> Maybe a
known (Known a)   = Just a
known (Unknown _) = Nothing

unknown :: Knowledge a -> Maybe a
unknown (Known _)   = Nothing
unknown (Unknown a) = Just a

type KnownEffects = ([Knowledge FluentPredicate], [Knowledge FluentPredicate])

-- Refactor into zipped list?
type ActionKnowledge = Map Name KnownEffects

-- fix 'knownPos' name, misleading
updateKnowledge :: Domain -> KnownEffects -> Action -> ActionSpec -> State -> State -> KnownEffects
updateKnowledge domain (knownPos, knownNeg) action aSpec oldState newState =
    let del = Set.toList $ oldState \\ newState
        add = Set.toList $ newState \\ oldState

        -- rewrite this
        unground' :: [GroundedPredicate] -> [Knowledge FluentPredicate]
        unground' = map Known
                  . Set.toList
                  . Set.unions
                  . (map $ unground aSpec (dmConstants domain) action)

        shouldUpdateKnowledge (Known a) (Unknown b) = a == b
        shouldUpdateKnowledge _ _ = False

        knownPos' = unionBy shouldUpdateKnowledge (unground' add) knownPos

    in (knownPos', knownNeg ++ unground' del)

constructSchema :: KnownEffects -> ActionSpec -> ActionSpec
constructSchema (posKnowledge, negKnowledge) action =
    let posEffects = map (Predicate . extract) posKnowledge
        negEffects = map (Neg . Predicate) (mapMaybe known negKnowledge)
        effect     = Con $ posEffects ++ negEffects
    in ActionSpec { asName    = asName action
                  , asParas   = asParas action
                  , asPrecond = asPrecond action
                  , asEffect  = effect
                  }

constructAllSchemas :: [ActionSpec] -> ActionKnowledge -> [ActionSpec]
constructAllSchemas aSpecs knowledge =
    let knowledgeForAction a = knowledge ! (asName a)
        act a = constructSchema (knowledgeForAction a) a
    in  map act $ aSpecs

analyzePlan :: Domain
            -> [Action]
            -> (State -> Action -> Maybe State)
            -> State
            -> ActionKnowledge
            -> (ActionKnowledge, Maybe State)

analyzePlan _ [] _ _ knowledge = (knowledge, Nothing)
analyzePlan domain (action : rest) evalAction oldState knowledge =
    if planState == actualState
    then analyzePlan domain rest evalAction actualState knowledge
    else (Map.insert (fst action) newKnowledge knowledge, Just actualState)
        where newKnowledge = updateKnowledge domain actionKnowledge action aSpec oldState actualState
              planState    = fromJust $ apply domain oldState action -- verify that this can never be Nothing
              actualState  = fromJust $ evalAction oldState action -- same as above
              aSpec = fromJust $ find (\a -> (asName a == fst action)) $ dmActionsSpecs domain
              actionKnowledge = (knowledge ! (fst action))

effectLearn :: Planner -> [Problem] -> Domain -> ()
effectLearn planner episodes domain = undefined
