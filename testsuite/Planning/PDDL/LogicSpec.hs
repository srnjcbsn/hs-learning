module Planning.PDDL.LogicSpec (main, spec) where

import           Control.Exception
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Logic.Formula
import           Planning.PDDL
import           Planning.PDDL.Logic

import           Data.Map as Map
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

getAction obj = ("testAction", [obj])

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
getGroundPred objs = Predicate "testPred1" objs


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

actionEffectA :: Effect
actionEffectA = ELit $ Pos getPred

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
            actSpec = getActSpec [] [ELit $ Neg getPred]
            domain = getDomain [actSpec]
            action = getAction testObj in
          apply domain initState action `shouldBe` Just emptyState

      it "applies the negative effects first, then the positive" $
        let initState = Set.empty
            actSpec = getActSpec [] [actionEffectA, ELit $ Neg $ getPred] -- same predicate for pos and neg
            domain = getDomain [actSpec]
            action = getAction testObj
            expectedState = getState [getGroundPred [testObj]]
        in
          apply domain initState action `shouldBe` Just expectedState

      it "gives nothing when the action's preconditions are not satisfied" $
        let actSpec = getActSpec [actionConditionA] []
            domain = getDomain [actSpec]
            action = getAction testObj in
          apply domain emptyState action `shouldBe` Nothing

-- TODO: Re-implement these tests
    -- describe "isActionValid" $ do
    --   it "can check if a positive precondition is in the state" $
    --     let initState = getState [getGroundPred [testObj]]
    --         groundAct = getGroundAct (getGroundPred [testObj]) [] [] in
    --       isActionValid initState groundAct `shouldBe` True
    --
    --   it "can check if a positive precondition is not in the state" $
    --     let groundAct = getGroundAct (GLit $ getGroundPred [testObj]) [] [] in
    --       isActionValid emptyState groundAct `shouldBe` False
    --
    --   it "can check if a negative precondition is in the state" $
    --     let initState = getState [getGroundPred [testObj]]
    --         groundAct = getGroundAct (GLit $ Neg $ getGroundPred [testObj]) [] [] in
    --       isActionValid initState groundAct `shouldBe` False
    --
    --   it "can check if a negative precondition is not in the state" $
    --     let groundAct = getGroundAct (GLit $ Neg $ getGroundPred [testObj]) [] [] in
    --       isActionValid emptyState groundAct `shouldBe` True
spec :: Spec
spec = testLogicSpec

main :: IO ()
main = hspec spec
