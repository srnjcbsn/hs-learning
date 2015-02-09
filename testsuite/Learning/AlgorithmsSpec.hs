module Learning.AlgorithmsSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)
import Data.Maybe (fromJust)

import PDDL.Samples.SimpleBox
import Learning.Algorithms
import PDDL.Type
import PDDL.Logic

p x y = ("p", [x, y])
pP x y = Predicate $ p x y

x = "?x"
y = "?y"
z = "?z"

xr = Ref "?x"
yr = Ref "?y"
zr = Ref "?z"

as = ActionSpec
 { asName = "as"
 , asParas = [x, y, z]
 , asPrecond = Con []
 , asEffect = Con [pP xr yr, pP yr zr]
 }

ambiDomain = Domain
    { dmName = "ambiDomain"
    , dmPredicates = [p x y]
    , dmActionsSpecs = [as]
    , dmConstants = []
    }

a1 = (asName as, ["a", "a", "b"])
a2 = (asName as, ["a", "b", "b"])
a3 = (asName as, ["b", "a", "b"])

s0 = Set.empty
s1 = fromJust $ apply ambiDomain s0 a1
s2 = fromJust $ apply ambiDomain s1 a2
s3 = fromJust $ apply ambiDomain s2 a3

t1 = (s0, s1, a1)
t2 = (s1, s2, a2)
t3 = (s2, s3, a3)

dh1 = initialHypothesis ambiDomain
dh2 = updateDomainHyp dh1 t1
dh3 = updateDomainHyp dh2 t2
dh4 = updateDomainHyp dh3 t3

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
            in do sortDomainAcSpecs (fst res) `shouldBe` sortDomainAcSpecs newDom
                  snd res `shouldBe` newKn
        it "can correctly handle ambiguos predicates" $ do
            domain dh2 `shouldBe` ambiDomain


spec :: Spec
spec = testEffectLearnSpec

main :: IO ()
main = hspec spec
