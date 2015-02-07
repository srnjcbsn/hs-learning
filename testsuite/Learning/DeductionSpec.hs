module Learning.DeductionSpec (main, spec) where

import           Learning.Deduction
import           PDDL.Type
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import qualified Data.Set  as Set
import           Data.Set  (Set)
import qualified Data.List as List


getArgs = List.map snd
getNames = List.map fst

testLogicSpec :: Spec
testLogicSpec = do
    describe "deduct" $ do
      it "can deduct all parameter posibilities when two arguments are the same" $
        let paras = ["x", "y"]
            actArgs = ["1", "1"]
            objs = ["1", "1"]
            expected = [ Set.fromList [Right "x", Right "y"], Set.fromList [Right "x", Right "y"] ] in
            deduct paras actArgs objs `shouldBe` expected

      it "can deduct the exact parameters when two arguments are different" $
        let paras = ["x", "y"]
            actArgs = ["1", "2"]
            objs = ["1", "2"]
            expected = [ Set.fromList [Right "x"], Set.fromList [ Right "y"] ] in
            deduct paras actArgs objs `shouldBe` expected

      it "ignores when object is not a parameter" $
        let paras = ["x"]
            actArgs = ["x"]
            objs = ["1"]
            expected = [ Set.fromList [Left "1"] ] in
            deduct paras actArgs objs `shouldBe` expected

    describe "asPDDL" $ do
      it "can turn deduct output into pddl format" $
        let deduction = [ Set.fromList [Right "x"], Set.fromList [Left "A"] ] in
            asPDDL deduction `shouldBe` [Set.fromList [Ref "x"], Set.fromList [Const "A"]]

    describe "variants" $ do
      it "can find all possible combinations" $
        let argumentOptions = [ Set.fromList ["x", "y"], Set.fromList ["x", "y"] ]
            expected = Set.fromList [["x", "x"], ["y", "y"], ["x", "y"], ["y", "x"]]
            actual = Set.fromList $ variants argumentOptions in
            actual `shouldBe` expected

    describe "unambiguate" $ do
      it "can find that a predicate is unambigous" $
        let allPreds = Set.fromList [("p",[Ref "x", Ref "y"]), ("p",[Ref "y", Ref "y"])]
            checkingPreds = Set.fromList [("p",[Ref "x", Ref "y"]), ("p",[Ref "x", Ref "x"])]
            actual = unambiguate allPreds checkingPreds in
            actual `shouldBe` Just ("p",[Ref "x", Ref "y"])

      it "can find that a predicate is not unambigous" $
        let allPreds = Set.fromList [("p",[Ref "x", Ref "y"]), ("p",[Ref "y", Ref "y"])]
            checkingPreds = Set.fromList [("p",[Ref "x", Ref "y"]), ("p",[Ref "y", Ref "y"])]
            actual = unambiguate allPreds checkingPreds in
            actual `shouldBe` Nothing

    describe "reducePossibilities" $ do
      it "can reduce the set of possibilities using a list all the ungrounded predicates" $
        let allPreds = Set.fromList [("p",[Ref "x", Ref "y"]), ("q",[Ref "y", Ref "y"]),  ("p",[Ref "x", Ref "x"])]
            predsToRemove = [Set.fromList [("p",[Ref "x", Ref "y"])], Set.fromList [("q",[Ref "y", Ref "y"])]]
            expected = Set.fromList [("p",[Ref "x", Ref "x"])]
            actual = reducePossibilities allPreds predsToRemove in
            actual `shouldBe` expected

spec :: Spec
spec = testLogicSpec

main :: IO ()
main = hspec spec
