module Learning.InductionSpec (main, spec) where

import qualified Data.Set              as Set
import           Learning.Induction
import           Logic.Formula
import           Planning.PDDL
import           Test.Hspec

p :: (Name -> Term) -> Name -> Name -> Predicate Term
p f x y = Predicate "p" [f x,f y]

q :: (Name -> Term) -> Name -> Name -> Predicate Term
q f x y = Predicate "q" [f x,f y]


testLogicSpec :: Spec
testLogicSpec = do
    describe "deduct" $ do
      it "can deduct all parameter posibilities when two arguments are the same" $
        let paras = ["x", "y"]
            actArgs = ["1", "1"]
            objs = ["1", "1"]
            expected = [ Set.fromList [Right "x", Right "y"], Set.fromList [Right "x", Right "y"] ] in
            induct paras actArgs objs `shouldBe` expected

      it "can deduct the exact parameters when two arguments are different" $
        let paras = ["x", "y"]
            actArgs = ["1", "2"]
            objs = ["1", "2"]
            expected = [ Set.fromList [Right "x"], Set.fromList [ Right "y"] ] in
            induct paras actArgs objs `shouldBe` expected

      it "ignores when object is not a parameter" $
        let paras = ["x"]
            actArgs = ["x"]
            objs = ["1"]
            expected = [ Set.fromList [Left "1"] ] in
            induct paras actArgs objs `shouldBe` expected

    describe "asPDDL" $ 
      it "can turn deduct output into pddl format" $
        let deduction = [ Set.fromList [Right "x"], Set.fromList [Left "A"] ] in
            asPDDL deduction `shouldBe` [Set.fromList [TVar "x"], Set.fromList [TName "A"]]

    describe "variants" $
      it "can find all possible combinations" $
        let argumentOptions = [ Set.fromList ["x", "y"], Set.fromList ["x", "y"] ]
            expected = Set.fromList [["x", "x"], ["y", "y"], ["x", "y"], ["y", "x"]]
            actual = Set.fromList $ variants argumentOptions in
            actual `shouldBe` expected

    describe "unambiguate" $ do
      it "can find that a predicate is unambigous" $
        let allPreds = Set.fromList [p TVar "x" "y", p TVar "y" "y"]
            checkingPreds = Set.fromList [ p TVar "x" "y"
                                         , p TVar "x" "x"]
            actual = unambiguate allPreds checkingPreds in
            actual `shouldBe` Left (p TVar "x" "y")

      it "can find that a predicate is not unambigous" $
        let allPreds = Set.fromList [p TVar "x" "y", p TVar "y" "y"]
            checkingPreds = Set.fromList [p TVar "x" "y", p TVar "y" "y"]
            actual = unambiguate allPreds checkingPreds in
            actual `shouldBe` Right checkingPreds

    describe "reducePossibilities" $
      it "can reduce the set of possibilities using a list all the ungrounded predicates" $
        let allPreds = Set.fromList [ p TVar "x" "y"
                                    , q TVar "y" "y"
                                    , p TVar "x" "x"
                                    ]

            predsToRemove = [Set.fromList [ p TVar "x" "y"
                                          , q TVar "y" "y"
                                          ]
                            ]
            expected = Set.fromList [p TVar "x" "x"]
            actual = reducePossibilities allPreds predsToRemove in
            actual `shouldBe` expected

spec :: Spec
spec = testLogicSpec

main :: IO ()
main = hspec spec
