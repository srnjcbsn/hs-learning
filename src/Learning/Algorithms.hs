module Learning.Algorithms where

import Data.Map (Map, (!))
import Data.Set (Set, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import PDDL.Type
import Learning.Deduction
import Data.Maybe (mapMaybe)

data Knowledge a = Known a
                 | Unknown a

extract :: Knowledge a -> a
extract (Known a)   = a
extract (Unknown a) = a

known :: Knowledge a -> Maybe a
known (Known a)   = Just a
known (Unknown a) = Nothing

unknown :: Knowledge a -> Maybe a
unknown (Known a)   = Nothing
unknown (Unknown a) = Just a

type KnownEffects = ([Knowledge FluentPredicate], [Knowledge FluentPredicate])

type ActionKnowledge = Map Name KnownEffects

updateKnowledge :: Action -> ActionSpec -> State -> State -> KnownEffects
updateKnowledge action aSpec oldState newState = undefined
    -- let del = oldState \\ newState
    --     add = newState \\ oldState
    --
    --     Set.unions $ map unground (asParas aSpec) action

-- updateKnowledge :: [FluentPredicate] -> KnownEffects -> KnownEffects
-- updateKnowledge fluents effects =
--     undefined

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
    in map act $ aSpecs

effectLearn :: Planner -> [Problem] -> Domain -> ()
effectLearn planner episodes domain = undefined
