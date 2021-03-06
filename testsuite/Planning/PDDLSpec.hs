module Planning.PDDLSpec (main, spec) where

-- import           Control.Exception
-- import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Logic.Formula
import           Planning.PDDL

import           Data.Map      as Map hiding (map)
import           Test.Hspec    hiding (context)
-- import           Test.Hspec.QuickCheck
-- import           Test.QuickCheck



testLogicSpec :: Spec
testLogicSpec =
  let getAction obj = ("testAction", [obj])
      getState :: [GroundedPredicate] -> State
      getState = Set.fromList

      getPred = Predicate "testPred1" [TVar "x"]
      fluPred vars = Predicate "testPred1" (map TVar vars)

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
              domainA = getDomain [actSpec]
              action = getAction testObj
              expectedState = getState [getGroundPred [testObj]]
           in apply domainA prob emptyState action `shouldBe` Just expectedState

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

      describe "isActionSatisfied" $
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
      describe "Conditional Effects" $
        let con allobjs= context allobjs [] []
            o1 = "o1"
            o2 = "o2"
            o3 = "o3"
            con2 = con [o1, o2]
            con3 = con [o1, o2, o3]
         in do
          it "Can add all effects when there are no conditions" $
           let eff = EForall ["x", "y"]
                      (EWhen (GAnd [])
                             (ePos $ fluPred ["x", "y"]) )

               expected =
                 ( Set.fromList [ getGroundPred [o1, o1]
                                , getGroundPred [o1, o2]
                                , getGroundPred [o2, o1]
                                , getGroundPred [o2, o2]
                                ]
                 , Set.empty
                 )
               actual = instantiateEffects con2 emptyState eff
            in actual `shouldBe` expected

          it "Can add effects when there are negative conditions" $
           let eff = EForall ["x", "y"]
                      (EWhen (gNeg $ fluPred ["x", "y"])
                             (ePos $ fluPred ["x", "y"]) )

               state = getState [ getGroundPred [o1, o1]
                                , getGroundPred [o2, o1]
                                ]
               expected =
                 ( Set.fromList [ getGroundPred [o1, o2]
                                , getGroundPred [o2, o2]
                                ]
                 , Set.empty
                 )
               actual = instantiateEffects con2 state eff
            in actual `shouldBe` expected

          it "Can add no effects when no conditions are satisfied" $
           let eff = EForall ["x", "y"]
                      (EWhen (GAnd [gPos $ fluPred ["x", "y"]])
                             (ePos $ fluPred ["x", "y"]) )

               expected =
                 ( Set.empty
                 , Set.empty
                 )
               actual = instantiateEffects con2 emptyState eff
            in actual `shouldBe` expected

          it "Can be effecient with many variables" $
           let vars = map show ([1..100] :: [Int])
               eff = EForall vars
                      (EWhen (gPos $ fluPred vars)
                             (ePos $ fluPred vars) )
               con100 = con vars
               state = getState [getGroundPred vars]
               expected =
                 ( Set.fromList [ getGroundPred vars
                                ]
                 , Set.empty
                 )
               actual = instantiateEffects con100 state eff
            in actual `shouldBe` expected

          it "Can handle multiple conditions" $
           let conditions = GAnd [  gPos $ fluPred ["x", "y"]
                                 ,  gPos $ fluPred ["y", "x"]]
               eff = EForall ["x", "y"]
                      (EWhen conditions
                             (ePos $ fluPred ["x", "x"]) )

               state = getState [ getGroundPred [o1, o2]
                                , getGroundPred [o3, o1]

                                , getGroundPred [o2, o3]
                                , getGroundPred [o3, o2]
                                ]
               expected =
                 ( Set.fromList [ getGroundPred [o2, o2]
                                , getGroundPred [o3, o3]
                                ]
                 , Set.empty
                 )
               actual = instantiateEffects con3 state eff
            in actual `shouldBe` expected
spec :: Spec
spec = testLogicSpec

main :: IO ()
main = hspec spec
