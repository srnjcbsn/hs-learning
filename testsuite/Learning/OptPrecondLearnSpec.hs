module Learning.OptPrecondLearnSpec where

import           Data.List                       (sort)
import           Data.Map                        (Map, (!))
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromJust)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import Learning.PDDL.PreconditionKnowledge
import qualified Data.TupleSet                   as TSet
-- import           Learning.OptPrecondLearn
import           Logic.Formula
import           Planning.PDDL
import Learning.PDDL.NonConditionalTypes
import        Learning.PDDL.NonConditionalKnowledge

p f x y = Predicate "p" [f x,f y]
q f x y = Predicate "q" [f x,f y]
pP x y = Pred $ p Ref x y

initActspec preconds = ActionSpec
    { asName = "as"
    , asParas = ["x", "y", "z"]
    , asPrecond = Con preconds
    , asEffect = Con []
    , asConstants = []
    , asTypes = Map.empty
    }

initDomain = PDDLDomain
    { dmName = "TestDomain"
    , dmPredicates = [p (flip (,) baseType) "x" "y"]
    , dmActionsSpecs = [initActspec []]
    , dmConstants = []
    , dmTypes = []
    }

precondKnl :: PDDLKnowledge env -> Name -> PreKnowledge
precondKnl pddlknl actname = preKnl
  where dk = domainKnowledge pddlknl
        (preKnl, _) = knlFromDomKnl dk actname



testPrecondLearnSpec :: Spec
testPrecondLearnSpec = do
    describe "update predicate hypothesis" $ do
        it "can prove that a positive predicate can not be a precondition" $
            let as = "as"
                s0 = Set.empty
                s1 = Set.fromList [p id "a" "b"]
                kn0 = PreKnowledge
                        Knowledge
                          { knowns = (Set.empty, Set.empty)
                          , unknowns = ( allPredsForAction initDomain as
                                       , Set.empty) }
                         Set.empty

                transition = (s0, (as, ["a", "b", "c"]), s1)
                kn1 = update initDomain kn0 transition

                actualUnknowns = (posUnknown . knlFromPk) kn1
                expectedUnknowns = Set.empty

             in actualUnknowns `shouldBe` expectedUnknowns
        it "can prove that a negative predicate can not be a precondition" $
            let as = "as"
                s0 = Set.singleton $ p id "a" "b"
                s1 = Set.empty
                kn0 = PreKnowledge
                        Knowledge
                          { knowns = (Set.empty, Set.empty)
                          , unknowns = ( Set.empty
                                       , allPredsForAction initDomain as) }
                         Set.empty

                transition = (s0, (as, ["a", "b", "c"]), s1)
                kn1 = update initDomain kn0 transition

                preknl = knlFromPk kn1
                actualUnknowns = negUnknown preknl

                expectedUnknowns = Set.delete (p Ref "x" "y") actualUnknowns
             in actualUnknowns `shouldBe` expectedUnknowns

        it "can prove that a positive predicate is a precondition when an action fails" $
            let as = "as"
                s0 = Set.empty
                posUnks = Set.fromList [p Ref "x" "y"]
                kn0 = PreKnowledge
                        Knowledge
                          { knowns = (Set.empty, Set.empty)
                          , unknowns = ( posUnks
                                       , Set.empty ) }
                         Set.empty

                action = (as, ["a", "b", "c"])
                transition = (s0, action, s0)
                kn1 = update initDomain kn0 transition

                preknl = knlFromPk kn1

                actualKnowns = posKnown preknl
                expectedKnowns = posUnks

             in  actualKnowns `shouldBe` expectedKnowns
        it "can prove that a positive predicate is a precondition when an action succeeds" $
            let as = "as"
                s = Set.fromList [p id "a" "b"]
                posCand = Set.fromList [p Ref "x" "y", p Ref "y" "x"]
                cand = Set.singleton (posCand, Set.empty)
                posUnks = posCand
                kn0 = PreKnowledge
                        Knowledge
                          { knowns = (Set.empty, Set.empty)
                          , unknowns = (posUnks, Set.empty) }
                         cand

                action = (as, ["a", "b", "c"])
                transition = (s, action, Set.empty)
                kn1 = update initDomain kn0 transition

                actualKnowns = (posKnown . knlFromPk) kn1

                expectedKnowns = Set.singleton $ p Ref "x" "y"

            in actualKnowns `shouldBe`expectedKnowns
        it "removes candidates containing a known positive precondition" $
            let as = "as"
                s = Set.fromList [p id "a" "b", p id "a" "c"]
                posCand = Set.fromList [p Ref "x" "y", p Ref "y" "x"]
                posRedundantCand = Set.fromList [p Ref "x" "y", p Ref "x" "z"]
                cand = Set.fromList [ (posCand, Set.empty)
                                    , (posRedundantCand, Set.empty)
                                    ]
                posUnks = posCand `Set.union` posRedundantCand
                kn0 = PreKnowledge
                        Knowledge
                          { knowns = TSet.empty
                          , unknowns = (posUnks, Set.empty) }
                         cand

                action = ("as", ["a", "b", "c"])
                transition = (s, action, Set.empty)
                actual = update initDomain kn0 transition


                expected = PreKnowledge
                            Knowledge
                              { knowns = ( Set.singleton $ p Ref "x" "y"
                                         , Set.empty)
                              , unknowns = ( Set.singleton $ p Ref "x" "z"
                                           , Set.empty) }
                              Set.empty

            in actual `shouldBe` expected
        it "can prove that a negative predicate is a precondition when an action fails" $
            let s = Set.singleton $ p id "a" "b"
                negUnks = Set.singleton $ p Ref "x" "y"
                kn0 = PreKnowledge
                        Knowledge
                          { knowns = TSet.empty
                          , unknowns = (Set.empty, negUnks) }
                          Set.empty

                action = ("as", ["a", "b", "c"])
                transition = (s, action, s)
                actual = update initDomain kn0 transition
                expected = PreKnowledge
                        Knowledge
                          { knowns = (Set.empty, negUnks)
                          , unknowns = TSet.empty }
                          Set.empty
             in  actual `shouldBe` expected
        it "can prove that a negative predicate is a precondition when an action succeeds" $
            let s = Set.singleton $ p id "b" "a"
                negCand = Set.fromList [p Ref "x" "y", p Ref "y" "x"]
                cand = Set.singleton (Set.empty, negCand)
                negUnks = negCand
                kn0 = PreKnowledge
                        Knowledge
                          { knowns = TSet.empty
                          , unknowns = (Set.empty, negUnks) }
                          cand
                action = ("as", ["a", "b", "c"])
                transition = (s, action, Set.empty)
                actual = update initDomain kn0 transition

                expected = PreKnowledge
                        Knowledge
                          { knowns = (Set.empty, Set.singleton $ p Ref "x" "y")
                          , unknowns = TSet.empty }
                          Set.empty

            in actual `shouldBe` expected
        it "removes candidates containing a known negative precondition" $
            let s = Set.singleton $ p id "b" "a"
                negCand = Set.fromList [p Ref "x" "y", p Ref "y" "x"]
                negRedundantCand = Set.fromList [p Ref "x" "y", p Ref "x" "z"]
                cand = Set.fromList [ (Set.empty, negCand)
                                    , (Set.empty, negRedundantCand)
                                    ]
                negUnks = negCand `Set.union` negRedundantCand

                kn0 = PreKnowledge
                        Knowledge
                          { knowns = TSet.empty
                          , unknowns = (Set.empty, negUnks) }
                          cand

                action = ("as", ["a", "b", "c"])
                transition = (s, action, Set.empty)
                actual = update initDomain kn0 transition

                expected = PreKnowledge
                        Knowledge
                          { knowns = (Set.empty, Set.singleton $ p Ref "x" "y")
                          , unknowns = (Set.empty, Set.singleton $ p Ref "x" "z") }
                          Set.empty

            in actual `shouldBe`expected



spec :: Spec
spec = testPrecondLearnSpec

main :: IO ()
main = hspec spec
