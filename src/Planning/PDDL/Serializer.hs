module Planning.PDDL.Serializer where

import           Logic.Formula
import           Planning      as Plng
import           Planning.PDDL

import           Data.List     (intercalate)
import qualified Data.Map      as Map
import qualified Data.Set      as Set

writeState :: State -> String
writeState state =
    unwords $ map writeGroundedPredicate $ Set.toList state

writeArgument :: Term -> String
writeArgument (TVar r)   = "?" ++ r
writeArgument (TName c) = c

writeArgumentList :: [Term] -> String
writeArgumentList as = unwords (map writeArgument as)

writeParameterList :: [String] -> String
writeParameterList ps = writeArgumentList $ map TVar ps

writeTypedList :: (Name -> String) -> [(Name, Type)] -> String
writeTypedList w ts@((_, t) : _) = sameList ++ " - " ++ t ++ " " ++ rest
    where sameList = unwords $ map (w . fst) same
          (same, different) = span ((t ==) . snd) ts
          rest = writeTypedList w different
writeTypedList _ [] = ""

writeTypedParameterList :: [(Name, Type)] -> String
writeTypedParameterList = writeTypedList (writeArgument . TVar)

writeFluentPredicate :: FluentPredicate -> String
writeFluentPredicate (Predicate pname as) =
    "(" ++ pname ++ " " ++ writeArgumentList as ++ ")"

writeGroundedPredicate :: GroundedPredicate -> String
writeGroundedPredicate (Predicate pname objs) =
    "(" ++ pname ++ " " ++ unwords objs  ++ ")"

writePredicateSpec :: PredicateSpec -> String
writePredicateSpec (Predicate pname ps) =
    "(" ++ pname ++ " " ++ writeTypedParameterList ps ++ ")"

writeActionSpec :: ActionSpec -> String
writeActionSpec as =
    "(:action " ++ asName as
    ++ "\t:parameters (" ++ params ++ ")\n"
    ++ "\t:precondition " ++ precond ++ "\n"
    ++ "\t:effect " ++ eff ++ "\n"
    ++ ")"
        where params  = writeTypedParameterList (Map.toList (asTypes as))
              precond = writeGoalDescription (asPrecond as)
              eff     = writeEffect (asEffect as)

writeGoalDescription :: GoalDesc -> String
writeGoalDescription = undefined

writeEffect :: Effect -> String
writeEffect = undefined

writeProblem :: PDDLProblem -> String
writeProblem prob =
    let defineStr = "(define (problem " ++ probName prob ++ ")"
        domStr    = "(:domain " ++ probDomain prob ++ ")"
        objs      = writeTypedList id $ Map.toList (probTypes prob)
        objsStr   = "(:objects " ++ objs ++ ")"
        initStr   = "(:init " ++ writeState (probState prob) ++ ")"
        goalStr   = "(:goal " ++ writeGoalDescription (probGoal prob) ++ ")"
    in intercalate "\n\t" [defineStr, domStr, objsStr, initStr, goalStr] ++ ")"


writeDomain :: PDDLDomain -> String
writeDomain domain =
    let defineStr = "(define (domain " ++ dmName domain ++ ")"
        reqsStr   = "(:requirements :strips :typing)"
        typesStr  = "(:types " ++ unwords (dmTypes domain) ++ ")"
        consts    = dmConstants domain
        constsStr = "(:constants " ++ unwords consts ++ ")"
        preds     = dmPredicates domain
        predsStr  = "(:predicates " ++ unwords (map writePredicateSpec preds) ++ ")"
        aSpecs    = unwords $ map writeActionSpec $ dmActionsSpecs domain
    in intercalate "\n\t" [ defineStr
                          , reqsStr
                          , typesStr
                          , constsStr
                          , predsStr
                          , aSpecs
                          ] ++ ")"
