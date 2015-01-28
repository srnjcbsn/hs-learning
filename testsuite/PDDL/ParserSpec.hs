module PDDL.ParserSpec (main, spec) where

import PDDL.Parser
import Test.Hspec
import Test.Hspec.QuickCheck

testParsePredicateSpec :: Spec
testParsePredicateSpec =
    describe "PDDL Parser" $
        it "can parse predicate specifications" $
            tryParse parsePredicateSpec "(a x y)" `shouldBe` Just ("a", ["x", "y"])



-- spec :: Spec
-- spec = do
--     describe "addition" $ do
--         it "can add two numbers" $ do
--             1 + 3 `shouldBe` 4
--         it "can add two floats" $ do
--             2.3 + 3.4 `shouldBe` 5.7

spec :: Spec
spec = do testParsePredicateSpec

main :: IO ()
main = hspec spec
