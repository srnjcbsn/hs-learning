module Learning.AlgorithmsSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

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

testEffectLearnSpec = do
    describe "initiate knowledge" $ do
        it "form an initial hypothesis for the SimpleBox domain" $ do
            fst (initialHypothesis sBDomain) `shouldBe` fst sBHyp
            snd (initialHypothesis sBDomain) `shouldBe` snd sBHyp

spec :: Spec
spec = testEffectLearnSpec

main :: IO ()
main = hspec spec
