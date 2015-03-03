module Learning.OptPrecondLearnSpec where

-- import           Data.List                       (sort)
-- import           Data.Map                        (Map, (!))
-- import qualified Data.Map                        as Map
-- import           Data.Maybe                      (fromJust)
-- import           Data.Set                        (Set)
-- import qualified Data.Set                        as Set
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
--
-- import qualified Data.TupleSet                   as TSet
-- import           Learning.OptPrecondLearn
-- import           Logic.Formula
-- import           Planning.PDDL

-- p f x y = Predicate "p" [f x,f y]
-- pP x y = Pred $ p Ref x y
--
-- initActspec preconds = ActionSpec
--     { asName = "as"
--     , asParas = ["x", "y", "z"]
--     , asPrecond = Con preconds
--     , asEffect = Con []
--     , asConstants = []
--     , asTypes = Map.empty
--     }
--
-- initDomain = PDDLDomain
--     { dmName = "TestDomain"
--     , dmPredicates = [p id "x" "y"]
--     , dmActionsSpecs = [initActspec []]
--     , dmConstants = []
--     , dmTypes = []
--     }
--
-- expected :: PreKnowledge
-- expected = (TSet.empty, TSet.empty, Set.empty)
--
-- testPrecondLearnSpec :: Spec
-- testPrecondLearnSpec = do
--     describe "update predicate hypothesis" $ do
--         it "can prove that a positive predicate can not be a precondition" $
--             let s0 = Set.empty
--                 s1 = Set.empty
--                 kn0 = initialPreDomainHyp initDomain
--                 transition = (s0, ("as", ["a", "b", "c"]), Just s1)
--                 expected = TSet.empty
--                 (actual, _, _) = updatePreDomainHyp initDomain kn0 transition ! "as"
--              in actual `shouldBe` expected
--         it "can prove that a negative predicate can not be a precondition" $
--             let s0 = Set.singleton $ p id "a" "b"
--                 s1 = Set.empty
--                 kn0 = initialPreDomainHyp initDomain
--                 (_, negs, _) = kn0 ! "as"
--                 transition = (s0, ("as", ["a", "b", "c"]), Just s1)
--                 expected = (Set.delete (p Ref "x" "y") $ (fst negs), Set.empty)
--                 (_, actual, _) = updatePreDomainHyp initDomain kn0 transition ! "as"
--              in actual `shouldBe` expected
--         it "can prove that a positive predicate is a precondition when an action fails" $
--             let s = Set.empty
--                 posUnks = Set.fromList [p Ref "x" "y"]
--                 kn0 = Map.singleton "as"
--                         ((posUnks, Set.empty), TSet.empty, Set.empty)
--                 action = ("as", ["a", "b", "c"])
--                 transition = (s, action, Nothing)
--                 actual = updatePreDomainHyp initDomain kn0 transition ! "as"
--                 expected = ((Set.empty, posUnks), TSet.empty, Set.empty)
--              in  actual `shouldBe`expected
--         it "can prove that a positive predicate is a precondition when an action succeeds" $
--             let s = Set.fromList [p id "a" "b"]
--                 posCand = Set.fromList [p Ref "x" "y", p Ref "y" "x"]
--                 cand = Set.singleton (posCand, Set.empty)
--                 posUnks = posCand
--                 kn = Map.singleton "as"
--                         ((posUnks, Set.empty), TSet.empty, cand)
--                 action = ("as", ["a", "b", "c"])
--                 transition = (s, action, Just Set.empty)
--                 actual = updatePreDomainHyp initDomain kn transition ! "as"
--                 expected = ( (Set.empty, Set.singleton $ p Ref "x" "y")
--                            , TSet.empty
--                            , Set.empty
--                            )
--             in actual `shouldBe`expected
--         it "removes candidates containing a known positive precondition" $
--             let s = Set.fromList [p id "a" "b", p id "a" "c"]
--                 posCand = Set.fromList [p Ref "x" "y", p Ref "y" "x"]
--                 posRedundantCand = Set.fromList [p Ref "x" "y", p Ref "x" "z"]
--                 cand = Set.fromList [ (posCand, Set.empty)
--                                     , (posRedundantCand, Set.empty)
--                                     ]
--                 posUnks = posCand `Set.union` posRedundantCand
--                 kn = Map.singleton "as"
--                         ((posUnks, Set.empty), TSet.empty, cand)
--                 action = ("as", ["a", "b", "c"])
--                 transition = (s, action, Just Set.empty)
--                 actual = updatePreDomainHyp initDomain kn transition ! "as"
--                 expected = ( (Set.singleton $ p Ref "x" "z"
--                              , Set.singleton $ p Ref "x" "y"
--                              )
--                            , TSet.empty
--                            , Set.empty
--                            )
--             in actual `shouldBe`expected
--         it "can prove that a negative predicate is a precondition when an action fails" $
--             let s = Set.singleton $ p id "a" "b"
--                 negUnks = Set.singleton $ p Ref "x" "y"
--                 kn0 = Map.singleton "as"
--                         (TSet.empty, (negUnks, Set.empty), Set.empty)
--                 action = ("as", ["a", "b", "c"])
--                 transition = (s, action, Nothing)
--                 actual = updatePreDomainHyp initDomain kn0 transition ! "as"
--                 expected = (TSet.empty, (Set.empty, negUnks), Set.empty)
--              in  actual `shouldBe` expected
--         it "can prove that a negative predicate is a precondition when an action succeeds" $
--             let s = Set.singleton $ p id "b" "a"
--                 negCand = Set.fromList [p Ref "x" "y", p Ref "y" "x"]
--                 cand = Set.singleton (Set.empty, negCand)
--                 negUnks = negCand
--                 kn = Map.singleton "as"
--                         (TSet.empty, (negUnks, Set.empty), cand)
--                 action = ("as", ["a", "b", "c"])
--                 transition = (s, action, Just Set.empty)
--                 actual = updatePreDomainHyp initDomain kn transition ! "as"
--                 expected = ( TSet.empty
--                            , (Set.empty, Set.singleton $ p Ref "x" "y")
--                            , Set.empty
--                            )
--             in actual `shouldBe` expected
--         it "removes candidates containing a known negative precondition" $
--             let s = Set.singleton $ p id "b" "a"
--                 negCand = Set.fromList [p Ref "x" "y", p Ref "y" "x"]
--                 negRedundantCand = Set.fromList [p Ref "x" "y", p Ref "x" "z"]
--                 cand = Set.fromList [ (Set.empty, negCand)
--                                     , (Set.empty, negRedundantCand)
--                                     ]
--                 negUnks = negCand `Set.union` negRedundantCand
--                 kn = Map.singleton "as"
--                         (TSet.empty, (negUnks, Set.empty), cand)
--                 action = ("as", ["a", "b", "c"])
--                 transition = (s, action, Just Set.empty)
--                 actual = updatePreDomainHyp initDomain kn transition ! "as"
--                 expected = ( TSet.empty
--                            , (Set.singleton $ p Ref "x" "z"
--                              , Set.singleton $ p Ref "x" "y"
--                              )
--                            , Set.empty
--                            )
--             in actual `shouldBe`expected


spec :: Spec
spec = describe "placeholder" $ it "is a placeholder" $ 1 `shouldBe` 1 --testPrecondLearnSpec

main :: IO ()
main = hspec spec
