module Planning.PDDLSpec (main, spec) where

import           Control.Exception
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Logic.Formula
import           Planning.PDDL

import           Data.Map              as Map
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck



testLogicSpec :: Spec
testLogicSpec =
  let getAction obj = ("testAction", [obj])
      getState :: [GroundedPredicate] -> State
      getState = Set.fromList

      getPred = Predicate "testPred1" [TVar "x"]

      getActSpec :: [GoalDesc] -> [Effect] -> ActionSpec
      getActSpec conds effects = ActionSpec { asName = "testAction"
                               , asParas = ["x"]
                               , asPrecond = GAnd conds
                               , asEffect = EAnd effects
                               , asConstants = []
                               , asTypes = Map.empty
                               }
      getGroundPred :: [Object] -> Predicate Object
      getGroundPred = Predicate "testPred1"

      getGroundAct :: GoalDesc
                   -> [GroundedPredicate]
                   -> [GroundedPredicate]
                   -> (GoalDesc, (State, State))
      getGroundAct preCond posEff negEff = (preCond,(getState posEff,getState negEff))

      getDomain as = PDDLDomain { dmName = "testDomain"
                            , dmPredicates = []
                            , dmActionsSpecs = as
                            , dmConstants = []
                            , dmTypes = []
                            }

      emptyState = getState []
      testObj = "testObj1"
      actionConditionA :: GoalDesc
      actionConditionA = GLit $ Pos getPred

      prob = allObjsToProblem [testObj]

      actionEffectA :: Effect
      actionEffectA = ELit $ Pos getPred
   in do
      describe "Apply" $ do
        it "can add effects to state" $
          let actSpec = getActSpec [] [actionEffectA]
              domain = getDomain [actSpec]
              action = getAction testObj
              expectedState = getState [getGroundPred [testObj]]
           in apply domain prob emptyState action `shouldBe` Just expectedState

        it "can remove effects to state" $
          let initState = getState [getGroundPred [testObj]]
              actSpec = getActSpec [] [ELit $ Neg getPred]
              domain = getDomain [actSpec]
              action = getAction testObj
          in  apply domain prob initState action `shouldBe` Just emptyState

        it "applies the negative effects first, then the positive" $
          let initState = Set.empty
              actSpec = getActSpec [] [actionEffectA, ELit $ Neg getPred] -- same predicate for pos and neg
              domain = getDomain [actSpec]
              action = getAction testObj
              expectedState = getState [getGroundPred [testObj]]
           in apply domain prob initState action `shouldBe` Just expectedState

        it "gives nothing when the action's preconditions are not satisfied" $
          let actSpec = getActSpec [actionConditionA] []
              domain = getDomain [actSpec]
              action = getAction testObj
           in apply domain prob emptyState action `shouldBe` Nothing

      describe "isActionValid" $
        let action = getAction testObj
            isActionSatisfied' = isActionSatisfied []
         in do
          it "can check if a positive precondition is in the state" $
            let s = getState [getGroundPred [testObj]]
                actSpec = getActSpec [gPos getPred] []
             in isActionSatisfied' actSpec action s  `shouldBe` True

          it "can check if a positive precondition is not in the state" $
            let actSpec = getActSpec [gPos getPred] []
             in isActionSatisfied' actSpec action emptyState  `shouldBe` False

          it "can check if a negative precondition is in the state" $
            let s = getState [getGroundPred [testObj]]
                actSpec = getActSpec [gNeg getPred] []
             in isActionSatisfied' actSpec action s  `shouldBe` False

          it "can check if a negative precondition is not in the state" $
            let actSpec = getActSpec [gNeg getPred] []
             in isActionSatisfied' actSpec action emptyState  `shouldBe` True
spec :: Spec
spec = testLogicSpec

main :: IO ()
main = hspec spec
