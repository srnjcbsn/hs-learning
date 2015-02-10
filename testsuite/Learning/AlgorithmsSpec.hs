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
a4 = (asName as, ["d", "c", "d"])

s0 = Set.empty
s1 = fromJust $ apply ambiDomain s0 a1
s2 = fromJust $ apply ambiDomain s1 a2
s3 = fromJust $ apply ambiDomain s2 a3
s4 = fromJust $ apply ambiDomain s3 a4

t1 = (s0, s1, a1)
t2 = (s1, s2, a2)
t3 = (s2, s3, a3)
t4 = (s3, s4, a4)

dh0 = initialHypothesis ambiDomain
dh1 = updateActionHypHelper dh0 t1
dh2 = updateActionHypHelper dh1 t2
dh3 = updateActionHypHelper dh2 t3
dh4 = updateActionHypHelper dh3 t4

-- putIn' = putIn
--     { asEffect = Con [pInside ar, pOutside ar] }
--
-- takeOut' = takeOut
--     { asEffect = Con [pInside ar, pOutside ar] }
--
-- sBDomain' = sBDomain { dmActionsSpecs = [putIn', takeOut'] }
--
-- sBEffKnowledge :: EffectKnowledge
-- sBEffKnowledge = ( Set.fromList [ inside ar, outside ar ]
--                  , Set.empty
--                  )
--
-- sBActKnowledge :: ActionKnowledge
-- sBActKnowledge = (sBEffKnowledge, sBEffKnowledge)
--
-- sBDomKnowledge :: DomainKnowledge
-- sBDomKnowledge = Map.fromList [ (asName putIn, sBActKnowledge)
--                               , (asName takeOut, sBActKnowledge)
--                               ]
--
-- sBHyp :: DomainHypothesis
-- sBHyp = (sBDomain', sBDomKnowledge)

sortDomainAcSpecs dom = dom { dmActionsSpecs = sort $ dmActionsSpecs dom }

testEffectLearnSpec = do
    -- describe "initiate knowledge" $
    --     it "can form an initial hypothesis for the SimpleBox domain" $ do
    --         fst (initialHypothesis sBDomain) `shouldBe` fst sBHyp
    --         snd (initialHypothesis sBDomain) `shouldBe` snd sBHyp
    describe "update domain hypothesis" $ do
        -- it "can correctly update a the domain hypothesis for the SimpleBox domain, given a state transition" $
        --     let oldState = Set.singleton $ inside a
        --         newState = Set.singleton $ outside a
        --         action = (asName takeOut, [a])
        --         transition = (oldState, newState, action)
        --         initH = initialHypothesis sBDomain
        --         newPosKn = ( (Set.empty, Set.singleton $ outside ar)
        --                    , (Set.empty, Set.singleton $ inside ar)
        --                    )
        --         newKn = Map.insert (asName takeOut) newPosKn sBDomKnowledge
        --         newDom = domainFromKnowledge sBDomain newKn
        --         res = updateDomainHyp initH transition
        --     in do snd res `shouldBe` newKn
        --           sortDomainAcSpecs (fst res) `shouldBe` sortDomainAcSpecs newDom

        it "can correctly handle ambiguos predicates" $ do
            domain dh2 `shouldBe` ambiDomain


spec :: Spec
spec = testEffectLearnSpec

main :: IO ()
main = hspec spec
