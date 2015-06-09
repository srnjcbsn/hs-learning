module Learning.OptEffectLearnSpec where

import qualified Data.Map                              as Map
import           Data.Maybe                            (fromJust)
import qualified Data.Set                              as Set
import           Test.Hspec

import           Data.TupleSet                         (TupleSet)
import           Learning.PDDL.NonConditionalKnowledge
import           Learning.PDDL.NonConditionalTypes
import           Logic.Formula
import           Planning.PDDL
import           Planning.PDDL.Logic


testEffectLearnSpec :: Spec
testEffectLearnSpec = do
        let p :: (Name -> t) -> Name -> Name -> Predicate t
            p f x y = Predicate "p" [f x,f y]

            initActSpec :: [Effect] -> ActionSpec
            initActSpec effects = ActionSpec
                { asName = "as"
                , asParas = ["x", "y", "z"]
                , asPrecond = GAnd []
                , asEffect = EAnd effects
                , asConstants = []
                , asTypes = Map.empty
                }

            initDomain :: ActionSpec -> PDDLDomain
            initDomain as = PDDLDomain
                { dmName = "TestDomain"
                , dmPredicates = [p (flip (,) baseType) "x" "y"]
                , dmActionsSpecs = [as]
                , dmConstants = []
                , dmTypes = []
                }
            allobjs = ["a", "b", "c", "d", "e", "f"]
            prob = allObjsToProblem allobjs

            actionPosEffectKnl :: PDDLKnowledge env -> Name -> TupleSet FluentPredicate
            actionPosEffectKnl pk name =
              let dk = domainKnowledge pk
                  (_, effknl) = knlFromDomKnl dk name
                  knl = knlFromEk effknl

              in (posUnknown knl, posKnown knl)

            actionNegEffectKnl :: PDDLKnowledge env -> Name -> TupleSet FluentPredicate
            actionNegEffectKnl pk name =
                let dk = domainKnowledge pk
                    (_, effknl) = knlFromDomKnl dk name
                    knl = knlFromEk effknl

                in (negUnknown knl, negKnown knl)
         in do
            it "can correctly handle ambiguos positive predicates" $ do
              let x = "x"
                  y = "y"
                  z = "z"
                  -- Effect is P(x,y) P(y,z):
                  actSpec = initActSpec [ePos $ p TVar x y, ePos $ p TVar y z]
                  actName = asName actSpec
                  a1 = (actName, ["a", "a", "b"])
                  a2 = (actName, ["c", "d", "d"])
                  a3 = (actName, ["e", "f", "e"])

                  ambiDomain = initDomain actSpec
                  -- updateActHyp = updateEffectHypHelper ambiDomain

                  s0 = Set.empty
                  s1 = fromJust $ apply ambiDomain prob s0 a1
                  s2 = fromJust $ apply ambiDomain prob s1 a2
                  s3 = fromJust $ apply ambiDomain prob s2 a3

                  t1 = (s0, a1, s1)
                  t2 = (s1, a2, s2)
                  t3 = (s2, a3, s3)



                  dh0 = initialKnowledge ambiDomain allobjs s0
                  dh1 = updateKnowledge dh0 t1
                  dh2 = updateKnowledge dh1 t2
                  dh3 = updateKnowledge dh2 t3

                  (unknown,known) = actionPosEffectKnl dh3 "as"
                  expectedKnown = Set.fromList [p TVar x y, p TVar y z]
                  expectedUnknown = Set.empty in do
                unknown `shouldBe` expectedUnknown
                known `shouldBe` expectedKnown

            it "can correctly handle ambiguos negative predicates" $ do
              let x = "x"
                  y = "y"
                  z = "z"
                  actSpec = initActSpec [eNeg $ p TVar x y, eNeg $ p TVar y z] -- Effect is P(x,y) P(y,z)
                  actName = asName actSpec
                  a1 = (actName, ["a", "a", "b"])
                  a2 = (actName, ["c", "d", "d"])
                  a3 = (actName, ["e", "f", "e"])

                  ambiDomain = initDomain actSpec

                  s0 = Set.fromList [ p id "a" "a"
                                    , p id "a" "b"
                                    , p id "c" "d"
                                    , p id "d" "d"
                                    , p id "e" "f"
                                    , p id "f" "e"
                                    ]

                  s1 = fromJust $ apply ambiDomain prob s0 a1
                  s2 = fromJust $ apply ambiDomain prob s1 a2
                  s3 = fromJust $ apply ambiDomain prob s2 a3

                  t1 = (s0, a1, s1)
                  t2 = (s1, a2, s2)
                  t3 = (s2, a3, s3)

                  dh0 = initialKnowledge ambiDomain allobjs s0
                  dh1 = updateKnowledge dh0 t1
                  dh2 = updateKnowledge dh1 t2
                  dh3 = updateKnowledge dh2 t3

                  (unknown,known) = actionNegEffectKnl dh3 "as"
                  expectedKnown = Set.fromList [p TVar x y, p TVar y z]
                  expectedUnknown = Set.empty in do
                unknown `shouldBe` expectedUnknown
                known `shouldBe` expectedKnown


spec :: Spec
spec = testEffectLearnSpec

main :: IO ()
main = hspec spec
