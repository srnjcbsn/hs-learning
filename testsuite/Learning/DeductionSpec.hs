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
testLogicSpec =
    describe "deduct" $ do
      it "can deduct all parameter posibilities when two arguments are the same" $
        let paras = ["x", "y"]
            actArgs = ["1", "1"]
            groundPref = ("_", ["1", "1"])
            expected = Set.fromList [ [Ref "x", Ref "y"], [Ref "y", Ref "x"], [Ref "x", Ref "x"], [Ref "y", Ref "y"] ]
            actual = Set.fromList $ getArgs $ deduct paras actArgs groundPref   in
            actual `shouldBe` expected

      it "can deduct the exact parameters when two arguments are different" $
        let paras = ["x", "y"]
            actArgs = ["1", "2"]
            groundPref = ("_", ["1", "2"])
            expected = [ [Ref "x", Ref "y"] ]
            actual = getArgs $ deduct paras actArgs groundPref   in
            actual `shouldBe` expected

      it "can deduct constants" $
        let groundPref = ("_", ["A"])
            expected = [ [Const "A"] ]
            actual = getArgs $ deduct [] [] groundPref   in
            actual `shouldBe` expected

      it "provides fluents with same name" $
        let predName = "testPred"
            groundPref = (predName, [])
            expected = [ predName ]
            actual = getNames $ deduct [] [] groundPref   in
            actual `shouldBe` expected

spec :: Spec
spec = testLogicSpec

main :: IO ()
main = hspec spec
