import PDDL
import Planning.FastDownward
import PDDL.Samples.SimpleBox
import           Data.List              (sort)
import           Data.Map               (Map, (!))
import qualified Data.Map               as Map
import           Data.Maybe             (fromJust)
import           Data.Set               (Set)
import qualified Data.Set               as Set

import qualified Data.TupleSet as TSet
import           Learning.OptPrecondLearn
import           PDDL.Logic
import           PDDL.Samples.SimpleBox
import           PDDL

main :: IO ()
main =
    let p f x y = ("p", [f x,f y])
        pP x y = Predicate $ p Ref x y

        initActspec preconds = ActionSpec
            { asName = "as"
            , asParas = ["x", "y", "z"]
            , asPrecond = Con preconds
            , asEffect = Con []
            }

        initDomain = Domain
            { dmName = "TestDomain"
            , dmPredicates = [p id "x" "y"]
            , dmActionsSpecs = [initActspec []]
            , dmConstants = []
            }
        s0 = Set.empty
        s1 = Set.empty
        kn0 = initialPreDomainHyp initDomain
        transition = (s0, ("as", ["a", "b", "c"]), Just s1)
        actualPre = updatePreDomainHyp initDomain kn0 transition
        actual = updatePreDomainHyp initDomain kn0 transition ! "as"
        expected :: PreKnowledge
        expected = (TSet.empty, TSet.empty, Set.empty)
    in putStrLn (show actual)
