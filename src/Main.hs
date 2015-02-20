module Main where

import Planning.FastDownward
import Environment
import Environment.Sokoban.SokobanDomain
import Environment.Sokoban.ConsoleView
import Environment.Sokoban.Samples.Sample1

main :: IO ()
main = undefined
    -- fd = mkFastDownard dom
    -- let p f x y = ("p", [f x,f y])
    --     pP x y = Predicate $ p Ref x y
    --
    --     initActspec preconds = ActionSpec
    --         { asName = "as"
    --         , asParas = ["x", "y", "z"]
    --         , asPrecond = Con preconds
    --         , asEffect = Con []
    --         }
    --
    --     initDomain = Domain
    --         { dmName = "TestDomain"
    --         , dmPredicates = [p id "x" "y"]
    --         , dmActionsSpecs = [initActspec []]
    --         , dmConstants = []
    --         }
    --     s0 = Set.empty
    --     s1 = Set.empty
    --     kn0 = initialPreDomainHyp initDomain
    --     transition = (s0, ("as", ["a", "b", "c"]), Just s1)
    --     actualPre = updatePreDomainHyp initDomain kn0 transition
    --     actual = updatePreDomainHyp initDomain kn0 transition ! "as"
    --     expected :: PreKnowledge
    --     expected = (TSet.empty, TSet.empty, Set.empty)
    -- in putStrLn (show actual)
