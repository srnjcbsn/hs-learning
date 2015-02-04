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

    describe "collectDeducts" $ do
      it "can combine two deductions" $
        let deduction1 = [ Set.fromList ["x", "y"], Set.singleton "y", Set.singleton "z" ]
            deduction2 = [ Set.singleton "x", Set.fromList ["x", "y"], Set.singleton "z" ]
            expected = [Set.singleton "x", Set.singleton "y", Set.singleton "z" ] in
            collectDeducts deduction1 deduction2 `shouldBe` expected

    describe "collectManyDeducts" $ do
      it "can combine multiple deductions" $
        let deduction1 = [ Set.fromList ["x", "y"]]
            deduction2 = [ Set.singleton "x"]
            expected = [Set.singleton "x" ] in
            collectManyDeduct [deduction1, deduction2] `shouldBe` expected

      it "can unpack single deduction" $
        let deduction = [Set.fromList ["x", "y"]]
            expected = [Set.fromList ["x", "y"]] in
            collectManyDeduct [deduction] `shouldBe` expected

      it "can handle deduction with no results" $
        let deduction = [Set.empty] :: [Set String]
            expected = [Set.empty] in
            collectManyDeduct [deduction] `shouldBe` expected

      it "can handle deduction with no arguments" $
        let deduction = [] :: [Set String]
            expected = [] in
            collectManyDeduct [deduction] `shouldBe` expected
spec :: Spec
spec = testLogicSpec

main :: IO ()
main = hspec spec
