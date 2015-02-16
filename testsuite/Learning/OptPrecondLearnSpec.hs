module Learning.OptPrecondLearnSpec where

import           Data.List              (sort)
import           Data.Map               (Map, (!))
import qualified Data.Map               as Map
import           Data.Maybe             (fromJust)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified Data.TupleSet as TSet
import           Learning.OptPrecondLearn
import           PDDL.Logic
import           PDDL.Samples.SimpleBox
import           PDDL.Type

p f x y = ("p", [f x,f y])
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

expected :: PreKnowledge
expected = (TSet.empty, TSet.empty, Set.empty)

testPrecondLearnSpec :: Spec
testPrecondLearnSpec = do
    describe "update predicate hypothesis" $ do
        it "can prove that a positive predicate can not be a precondition" $ do
            let s0 = Set.empty
                s1 = Set.empty
                kn0 = initialPreDomainHyp initDomain
                transition = (s0, ("as", ["a", "b", "c"]), Just s1)
                expected = TSet.empty
                (actual, _, _) = updatePreDomainHyp initDomain kn0 transition ! "as"
             in actual `shouldBe` expected
        it "can prove that a negative predicate can not be a precondition" $ do
            let s0 = Set.singleton $ p id "a" "b"
                s1 = Set.empty
                kn0 = initialPreDomainHyp initDomain
                (_, negs, _) = kn0 ! "as"
                transition = (s0, ("as", ["a", "b", "c"]), Just s1)
                expected = (Set.delete (p Ref "x" "y") $ (fst negs), Set.empty)
                (_, actual, _) = updatePreDomainHyp initDomain kn0 transition ! "as"
             in actual `shouldBe` expected

spec :: Spec
spec = testPrecondLearnSpec

main :: IO ()
main = hspec spec
