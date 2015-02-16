module Learning.OptEffectLearnSpec where

import           Data.List              (sort)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (fromJust)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Learning.OptEffectLearn
import           PDDL.Logic
import           PDDL.Samples.SimpleBox
import           PDDL.Type

p f x y = ("p", [f x,f y])
pP x y = Predicate $ p Ref x y

initActspec effects = ActionSpec
    { asName = "as"
    , asParas = ["x", "y", "z"]
    , asPrecond = Con []
    , asEffect = Con effects
    }



initDomain as = Domain
    { dmName = "TestDomain"
    , dmPredicates = [p id "x" "y"]
    , dmActionsSpecs = [as]
    , dmConstants = []
    }


actionPosEffects :: DomainHypothesis -> Name -> EffectKnowledge
actionPosEffects domainHyp name = fst $ domainHyp Map.! name

actionNegEffects :: DomainHypothesis -> Name -> EffectKnowledge
actionNegEffects domainHyp name = snd $ domainHyp Map.! name

putIn' = putIn
    { asEffect = Con [pInside ar, pOutside ar] }

takeOut' = takeOut
    { asEffect = Con [pInside ar, pOutside ar] }

sBDomain' = sBDomain { dmActionsSpecs = [putIn', takeOut'] }

sBEffKnowledge :: EffectKnowledge
sBEffKnowledge = ( Set.fromList [ inside ar, outside ar ]
                 , Set.empty
                 )

sBActKnowledge :: EffectHypothesis
sBActKnowledge = (sBEffKnowledge, sBEffKnowledge)

sBDomKnowledge :: DomainHypothesis
sBDomKnowledge = Map.fromList [ (asName putIn, sBActKnowledge)
                              , (asName takeOut, sBActKnowledge)
                              ]


sortDomainAcSpecs dom = dom { dmActionsSpecs = sort $ dmActionsSpecs dom }

testEffectLearnSpec = do
    describe "initiate knowledge" $
        it "can form an initial hypothesis for the SimpleBox domain" $ do
          let dHyp = initialHypothesis sBDomain in do
            dHyp `shouldBe` sBDomKnowledge

    describe "update domain hypothesis" $ do
        it "can correctly update a the domain hypothesis for the SimpleBox domain, given a state transition" $
            let oldState = Set.singleton $ inside a
                newState = Set.singleton $ outside a
                action = (asName takeOut, [a])
                transition = (oldState, action, Just newState)
                initH = initialHypothesis sBDomain
                expectedPosKn = ( (Set.empty, Set.singleton $ outside ar)
                                , (Set.empty, Set.singleton $ inside ar)
                                )
                expectedHyp = Map.insert (asName takeOut) expectedPosKn sBDomKnowledge
                expectedDomain = domainFromKnowledge sBDomain expectedHyp
                actualHyp = updateDomainHyp sBDomain expectedHyp transition -- ??? why does it use the expected what is it testing?
                actualDom = domainFromKnowledge sBDomain actualHyp
            in do actualHyp `shouldBe` expectedHyp
                  sortDomainAcSpecs actualDom `shouldBe` sortDomainAcSpecs expectedDomain

        it "can correctly handle ambiguos positive predicates" $ do
          let x = "x"
              y = "y"
              z = "z"
              actSpec = initActspec [pP x y, pP y z] -- Effect is P(x,y) P(y,z)
              actName = asName actSpec
              a1 = (actName, ["a", "a", "b"])
              a2 = (actName, ["c", "d", "d"])
              a3 = (actName, ["e", "f", "e"])

              ambiDomain = initDomain actSpec
              updateActHyp = updateEffectHypHelper ambiDomain

              s0 = Set.empty
              s1 = fromJust $ apply ambiDomain s0 a1
              s2 = fromJust $ apply ambiDomain s1 a2
              s3 = fromJust $ apply ambiDomain s2 a3

              t1 = (s0, a1, Just s1)
              t2 = (s1, a2, Just s2)
              t3 = (s2, a3, Just s3)

              dh0 = initialHypothesis ambiDomain
              dh1 = updateActHyp dh0 t1
              dh2 = updateActHyp dh1 t2
              dh3 = updateActHyp dh2 t3

              (unknown,known) = actionPosEffects dh3 "as"
              expectedKnown = Set.fromList [p Ref x y, p Ref y z]
              expectedUnknown = Set.empty in do
            unknown `shouldBe` expectedUnknown
            known `shouldBe` expectedKnown

        it "can correctly handle ambiguos negative predicates" $ do
          let x = "x"
              y = "y"
              z = "z"
              actSpec = initActspec [Neg $ pP x y, Neg $ pP y z] -- Effect is P(x,y) P(y,z)
              actName = asName actSpec
              a1 = (actName, ["a", "a", "b"])
              a2 = (actName, ["c", "d", "d"])
              a3 = (actName, ["e", "f", "e"])

              ambiDomain = initDomain actSpec
              updateActHyp = updateEffectHypHelper ambiDomain

              s0 = Set.fromList [ p id "a" "a"
                                , p id "a" "b"
                                , p id "c" "d"
                                , p id "d" "d"
                                , p id "e" "f"
                                , p id "f" "e"
                                ]

              s1 = fromJust $ apply ambiDomain s0 a1
              s2 = fromJust $ apply ambiDomain s1 a2
              s3 = fromJust $ apply ambiDomain s2 a3

              t1 = (s0, a1, Just s1)
              t2 = (s1, a2, Just s2)
              t3 = (s2, a3, Just s3)

              dh0 = initialHypothesis ambiDomain
              dh1 = updateActHyp dh0 t1
              dh2 = updateActHyp dh1 t2
              dh3 = updateActHyp dh2 t3

              (unknown,known) = actionNegEffects dh3 "as"
              expectedKnown = Set.fromList [p Ref x y, p Ref y z]
              expectedUnknown = Set.empty in do
            unknown `shouldBe` expectedUnknown
            known `shouldBe` expectedKnown


spec :: Spec
spec = testEffectLearnSpec

main :: IO ()
main = hspec spec
