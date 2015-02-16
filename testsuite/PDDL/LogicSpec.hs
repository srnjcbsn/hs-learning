module PDDL.LogicSpec (main, spec) where

import           Control.Exception
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           PDDL.Logic
import           PDDL.Type
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck




getAction obj = ("testAction", [obj])

getState :: [GroundedPredicate] -> State
getState = Set.fromList

getPred = Predicate ("testPred1", [Ref "x"])

getActSpec conds effects = ActionSpec { asName = "testAction"
                         , asParas = ["x"]
                         , asPrecond = Con conds
                         , asEffect = Con effects
                         }
getGroundPred objs = ("testPred1", objs)


getGroundAct posCond negCond posEff negEff = ((getState posCond,getState negCond),(getState posEff,getState negEff))

getDomain as = Domain { dmName = "testDomain"
                      , dmPredicates = []
                      , dmActionsSpecs = as
                      , dmConstants = []
                      }

emptyState = getState []
testObj = "testObj1"
actionCondionA = getPred
actionEffectA = getPred

testLogicSpec :: Spec
testLogicSpec = do
    describe "Apply" $ do
      it "can add effects to state" $
        let actSpec = getActSpec [] [actionEffectA]
            domain = getDomain [actSpec]
            action = getAction testObj
            expectedState = getState [getGroundPred [testObj]] in
          apply domain emptyState action `shouldBe` Just expectedState

      it "can remove effects to state" $
        let initState = getState [getGroundPred [testObj]]
            actSpec = getActSpec [] [Neg $ actionEffectA]
            domain = getDomain [actSpec]
            action = getAction testObj in
          apply domain initState action `shouldBe` Just emptyState

      it "applies the negative effects first, then the positive" $
        let initState = Set.empty
            actSpec = getActSpec [] [Neg $ actionEffectA, actionEffectA] -- same predicate for pos and neg
            domain = getDomain [actSpec]
            action = getAction testObj
            expectedState = getState [getGroundPred [testObj]]
        in
          apply domain initState action `shouldBe` Just expectedState

      it "gives nothing when the action's preconditions are not satisfied" $
        let actSpec = getActSpec [actionCondionA] []
            domain = getDomain [actSpec]
            action = getAction testObj in
          apply domain emptyState action `shouldBe` Nothing


    describe "isActionValid" $ do
      it "can check if a positive precondition is in the state" $
        let initState = getState [getGroundPred [testObj]]
            groundAct = getGroundAct [getGroundPred [testObj]] [] [] [] in
          isActionValid initState groundAct `shouldBe` True

      it "can check if a positive precondition is not in the state" $
        let groundAct = getGroundAct [getGroundPred [testObj]] [] [] [] in
          isActionValid emptyState groundAct `shouldBe` False

      it "can check if a negative precondition is in the state" $
        let initState = getState [getGroundPred [testObj]]
            groundAct = getGroundAct [] [getGroundPred [testObj]] [] [] in
          isActionValid initState groundAct `shouldBe` False

      it "can check if a negative precondition is not in the state" $
        let groundAct = getGroundAct [] [getGroundPred [testObj]] [] [] in
          isActionValid emptyState groundAct `shouldBe` True
spec :: Spec
spec = testLogicSpec

main :: IO ()
main = hspec spec
