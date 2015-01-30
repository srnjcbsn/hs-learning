module PDDL.LogicSpec (main, spec) where

import           PDDL.Logic
import           PDDL.Type
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import qualified Data.Set  as Set
import           Data.Set  (Set)

testObj = "testObj1"

actionInp = ("testAction", [testObj])

getPred id = Predicate ("testPred" ++ show id, [Ref "x"])

getActSpec conds effects  = ActionSpec { asName = "testAction"
                         , asParas = ["x"]
                         , asPrecond = Con conds
                         , asEffect = Con effects
                         }
getGroundPred id objs = ("testPred" ++ show id, objs)

getDomain as = Domain { dmPredicates = []
                    , dmActionsSpecs = as
                    , dmConstants = []
                    }

getState :: [GroundedPredicate] -> State
getState = Set.fromList

testLogicSpec :: Spec
testLogicSpec = do
    describe "Apply" $ do
      it "can add effects to state" $ do
        let initState = getState []
            actSpec = getActSpec [] [getPred 1]
            domain = getDomain [actSpec]
            expectedState = getState [getGroundPred 1 [testObj]] in
          apply domain initState actionInp `shouldBe` Just expectedState

      it "can remove effects to state" $ do
        let initState = getState [getGroundPred 1 [testObj]]
            expectedState = getState []
            actSpec = getActSpec [] [Neg $ getPred 1]
            domain = getDomain [actSpec] in
          apply domain initState actionInp `shouldBe` Just expectedState

spec :: Spec
spec = testLogicSpec

main :: IO ()
main = hspec spec
