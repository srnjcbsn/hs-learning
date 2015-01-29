module PDDL.ParserSpec (main, spec) where

import           PDDL.Parser
import           PDDL.Type
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

predStrA = "(a ?x Y)"
predResA = ("a", [Ref "x", Const "Y"])

predStrB = "(b ?z V)"
predResB = ("b", [Ref "z", Const "V"])

actionSpecStr = unlines [ "(:action act"
                        , ":parameters (?a)"
                        , ":precondition (p ?a)"
                        , ":effect (not (p ?a)) )"
                        ]

actionSpecRes = ActionSpec { asName = "act"
                           , asParas = ["a"]
                           , asPrecond = Predicate ("p", [Ref "a"])
                           , asEffect = Neg (Predicate ("p", [Ref "a"]))
                           }

domainSpecStr =
    unlines [ "(define (domain dom)"
            , "(:constants (A B))"
            , "(:predicates (a ?x ?y))"
            , actionSpecStr
            , ")"
            ]

domainSpecRes =
    Domain { dmPredicates = [("a", ["x", "y"])]
           , dmActionsSpecs = [actionSpecRes]
           , dmConstants = ["A", "B"]
           }

testParsePredicateSpec :: Spec
testParsePredicateSpec = do
    describe "Identifier parser" $ do
        it "can contain '-', '_', and numbers" $ do
            tryParse parseName "a-" `shouldBe` Just "a-"
            tryParse parseName "a_" `shouldBe` Just "a_"
            tryParse parseName "a1" `shouldBe` Just "a1"
            tryParse parseName "a1A-_" `shouldBe` Just "a1A-_"
        it "can only start with a lowercase letter" $ do
            tryParse parseName "-a" `shouldBe` Nothing
            tryParse parseName "_a" `shouldBe` Nothing
            tryParse parseName "1a" `shouldBe` Nothing
            tryParse parseName "Aa" `shouldBe` Nothing
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
        it "requires a lowercase letter after the '?'" $ do
            tryParse parseArgRef "?" `shouldBe` Nothing
            tryParse parseArgRef "?A" `shouldBe` Nothing

    describe "Constant parser" $ do
        it "requires the first letter to be uppercase" $
            tryParse parseConstant "a" `shouldBe` Nothing
        it "correctly parses proper constants" $
            tryParse parseConstant "A" `shouldBe` Just "A"

    describe "Argument parser" $ do
        it "parses a '?'-prefixed string as a reference" $
            tryParse parseArgument "?a" `shouldBe` Just (Ref "a")
        it "parses a string starting with a capital letter as a constant" $
            tryParse parseArgument "A" `shouldBe` Just (Const "A")
        it "fails to parse strings that are not prefixed by '?' or a capital letter" $
            tryParse parseArgument "a" `shouldBe` Nothing

    describe "Fluent parser" $ do
        it "can parse fluents with 'Argument's" $
            tryParse parseFluent predStrA `shouldBe` Just predResA

    describe "Formula parser" $ do
        it "can parse fluents" $
            tryParse parseFormula predStrA `shouldBe` Just (Predicate predResA)
        it "can parse negated formulae" $
            tryParse parseFormula ("(not" ++ predStrA ++ ")")
                `shouldBe` Just (Neg (Predicate predResA))
        it "can parse conjunctions" $
            tryParse parseFormula ("(and " ++ predStrA ++ " " ++ predStrB ++ ")")
                `shouldBe` Just (Con [Predicate predResA, Predicate predResB])

    describe "Action specification parser" $ do
        it "can parse basic action specifications" $
            tryParse parseActionSpec actionSpecStr `shouldBe` Just actionSpecRes

    describe "Domain definition parser" $ do
        it "can parse simple domains" $
            tryParse parseDomain domainSpecStr `shouldBe` Just domainSpecRes
spec :: Spec
spec = testParsePredicateSpec

main :: IO ()
main = hspec spec
