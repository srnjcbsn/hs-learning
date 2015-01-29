module PDDL.ParserSpec (main, spec) where

import PDDL.Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

testParsePredicateSpec :: Spec
testParsePredicateSpec = do
    describe "Predicate specification parser" $ do
        it "can parse predicate specifications" $
            tryParse parsePredicateSpec "(a ?x ?y)" `shouldBe` Just ("a", ["x", "y"])
        it "can parse predicate specs with no parameters" $ do
            tryParse parsePredicateSpec "(a)"  `shouldBe` Just ("a", [])
            tryParse parsePredicateSpec "(a )" `shouldBe` Just ("a", [])
    describe "Argument reference parser" $ do
        it "requires a '?' prefix" $
            tryParse parseArgRef "a" `shouldBe` Nothing
        it "throws away the prefix" $
            tryParse parseArgRef "?a" `shouldBe` Just "a"



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
