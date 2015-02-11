module Learning.AlgorithmsSpec where

import           Data.List              (sort)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (fromJust)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Learning.Algorithms
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
actionPosEffects domainHyp name = fst $ snd domainHyp Map.! name

actionNegEffects :: DomainHypothesis -> Name -> EffectKnowledge
actionNegEffects domainHyp name = snd $ snd domainHyp Map.! name

putIn' = putIn
    { asEffect = Con [pInside ar, pOutside ar] }

takeOut' = takeOut
    { asEffect = Con [pInside ar, pOutside ar] }

sBDomain' = sBDomain { dmActionsSpecs = [putIn', takeOut'] }

sBEffKnowledge :: EffectKnowledge
sBEffKnowledge = ( Set.fromList [ inside ar, outside ar ]
                 , Set.empty
                 )

sBActKnowledge :: ActionKnowledge
sBActKnowledge = (sBEffKnowledge, sBEffKnowledge)

sBDomKnowledge :: DomainKnowledge
sBDomKnowledge = Map.fromList [ (asName putIn, sBActKnowledge)
                              , (asName takeOut, sBActKnowledge)
                              ]

sBHyp :: DomainHypothesis
sBHyp = (sBDomain', sBDomKnowledge)

sortDomainAcSpecs dom = dom { dmActionsSpecs = sort $ dmActionsSpecs dom }

testEffectLearnSpec = do
    describe "initiate knowledge" $
        it "can form an initial hypothesis for the SimpleBox domain" $ do
          let (hypDomain,hypDKnowledge) = (initialHypothesis sBDomain) in do
            hypDomain `shouldBe` sBDomain'
            hypDKnowledge `shouldBe` sBDomKnowledge

    describe "update domain hypothesis" $ do
        it "can correctly update a the domain hypothesis for the SimpleBox domain, given a state transition" $
            let oldState = Set.singleton $ inside a
                newState = Set.singleton $ outside a
                action = (asName takeOut, [a])
                transition = (oldState, newState, action)
                initH = initialHypothesis sBDomain
                newPosKn = ( (Set.empty, Set.singleton $ outside ar)
                           , (Set.empty, Set.singleton $ inside ar)
                           )
                newKn = Map.insert (asName takeOut) newPosKn sBDomKnowledge
                newDom = domainFromKnowledge sBDomain newKn
                res = updateDomainHyp initH transition
            in do snd res `shouldBe` newKn
                  sortDomainAcSpecs (fst res) `shouldBe` sortDomainAcSpecs newDom

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

              s0 = Set.empty
              s1 = fromJust $ apply ambiDomain s0 a1
              s2 = fromJust $ apply ambiDomain s1 a2
              s3 = fromJust $ apply ambiDomain s2 a3

              t1 = (s0, s1, a1)
              t2 = (s1, s2, a2)
              t3 = (s2, s3, a3)

              dh0 = initialHypothesis ambiDomain
              dh1 = updateActionHypHelper dh0 t1
              dh2 = updateActionHypHelper dh1 t2
              dh3 = updateActionHypHelper dh2 t3

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

              t1 = (s0, s1, a1)
              t2 = (s1, s2, a2)
              t3 = (s2, s3, a3)

              dh0 = initialHypothesis ambiDomain
              dh1 = updateActionHypHelper dh0 t1
              dh2 = updateActionHypHelper dh1 t2
              dh3 = updateActionHypHelper dh2 t3

              (unknown,known) = actionNegEffects dh3 "as"
              expectedKnown = Set.fromList [p Ref x y, p Ref y z]
              expectedUnknown = Set.empty in do
            unknown `shouldBe` expectedUnknown
            known `shouldBe` expectedKnown


spec :: Spec
spec = testEffectLearnSpec

main :: IO ()
main = hspec spec
