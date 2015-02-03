module Learning.DeductionSpec (main, spec) where

import           Learning.Deduction
import           PDDL.Type
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import qualified Data.Set  as Set
import           Data.Set  (Set)
import qualified Data.List as List

paras = ["x", "y"]
getArgs = List.map snd
getNames = List.map fst

testLogicSpec :: Spec
testLogicSpec =
    describe "deduct" $ do
      it "can deduct all parameter posibilities when two arguments are the same" $
        let actArgs = ["1", "1"]
            groundPref = ("testPred", ["1", "1"])
            expected = Set.fromList [ [Ref "x", Ref "y"], [Ref "y", Ref "x"], [Ref "x", Ref "x"], [Ref "y", Ref "y"] ]
            actual = Set.fromList $ getArgs $ deduct paras actArgs groundPref   in
            actual `shouldBe` expected

      it "can deduct the exact parameters when two arguments are different" $
        let actArgs = ["1", "2"]
            paraMap = mapMany actArgs paras
            groundPref = ("testPred", ["1", "2"])
            expected = [ [Ref "x", Ref "y"] ]
            actual = getArgs $ deduct paras actArgs groundPref   in
            actual `shouldBe` expected

      it "can deduct constants" $
        let actArgs = ["_"]
            groundPref = ("testPred", ["_", "A"])
            expected = [ [Ref "x", Const "A"] ]
            actual = getArgs $ deduct paras actArgs groundPref   in
            actual `shouldBe` expected

      it "provides fluents with same name" $
        let actArgs = ["_"]
            groundPref = ("testPred", ["_"])
            expected = [ "testPred" ]
            actual = getNames $ deduct paras actArgs groundPref   in
            actual `shouldBe` expected

spec :: Spec
spec = testLogicSpec

main :: IO ()
main = hspec spec
