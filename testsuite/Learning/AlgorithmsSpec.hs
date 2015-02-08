module Learning.AlgorithmsSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)

import PDDL.Samples.SimpleBox
import Learning.Algorithms
import PDDL.Type

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
            fst (initialHypothesis sBDomain) `shouldBe` fst sBHyp
            snd (initialHypothesis sBDomain) `shouldBe` snd sBHyp
    describe "update domain hypothesis" $
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
            in do sortDomainAcSpecs (fst res) `shouldBe` sortDomainAcSpecs newDom
                  snd res `shouldBe` newKn

spec :: Spec
spec = testEffectLearnSpec

main :: IO ()
main = hspec spec
