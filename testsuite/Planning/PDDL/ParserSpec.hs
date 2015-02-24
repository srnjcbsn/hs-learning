module Planning.PDDL.ParserSpec (main, spec) where

import           Data.Char
import qualified Data.Set              as Set
import           Planning.PDDL
import           Planning.PDDL.Parser
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

actionSpecRes = ActionSpec { asName    = "act"
                           , asParas   = ["a"]
                           , asPrecond = Predicate ("p", [Ref "a"])
                           , asEffect  = Neg (Predicate ("p", [Ref "a"]))
                           }

domainSpecStr =
    unlines [ "(define (domain test)"
            , "(:requirements :strips)"
            , "(:constants A B)"
            , "(:predicates (a ?x ?y))"
            , actionSpecStr
            , ")"
            ]

domainSpecRes =
    PDDLDomain { dmName         = "test"
           , dmPredicates   = [("a", ["x", "y"])]
           , dmActionsSpecs = [actionSpecRes]
           , dmConstants    = ["A", "B"]
           }

problemSpecStr =
    unlines [ "(define (problem prob)"
            , "(:domain dom)"
            , "(:objects x y)"
            , "(:init (test1 x y))"
            , "(:goal (test2 x y)) )"
            ]

problemSpecRes =
    PDDLProblem { probName = "prob"
            , probObjs = ["x", "y"]
            , probDomain = "dom"
            , probState = Set.singleton ("test1", ["x", "y"])
            , probGoal = Predicate ("test2", [Const "x", Const "y"])
            }

testParsePredicateSpec :: Spec
testParsePredicateSpec = do
    describe "Identifier parser" $ do
        it "can contain '-', '_', and numbers" $ do
            tryParse parseName "a-" `shouldBe` Just "a-"
            tryParse parseName "a_" `shouldBe` Just "a_"
            tryParse parseName "a1" `shouldBe` Just "a1"
            tryParse parseName "a1A-_" `shouldBe` Just "a1A-_"
        it "can only start with a letter" $ property $
            \c -> let parsed = tryParse parseName (c : "a")
                  in  if isLetter c then parsed == Just (c : "a")
                      else parsed == Nothing

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
        it "requires a letter after the '?'" $ do
            tryParse parseArgRef "?" `shouldBe` Nothing

    describe "Argument parser" $ do
        it "parses a '?'-prefixed string as a reference" $
            tryParse parseArgument "?a" `shouldBe` Just (Ref "a")
        it "parses a string starting with a capital letter as a constant" $
            tryParse parseArgument "A" `shouldBe` Just (Const "A")

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

    describe "Plan parser" $ do
        it "can parse simple plans (as produced by fast-downward)" $
            let planStr = unlines [ "(push-h b0x0 b0x0 b1x0 b2x0)"
                                  , "(push-h b2x1 b2x2 boxat1x0 boxat2x1)"
                                  , "; cost = 2 (unit cost)"
                                  ]
                expectedPlan = [ ("push-h", ["b0x0", "b0x0", "b1x0", "b2x0"])
                               , ("push-h", ["b2x1", "b2x2", "boxat1x0", "boxat2x1"])
                               ]
            in tryParse plan planStr `shouldBe` Just expectedPlan

    describe "Action specification parser" $ do
        it "can parse basic action specifications" $
            tryParse parseActionSpec actionSpecStr `shouldBe` Just actionSpecRes

    describe "Domain definition parser" $ do
        it "can parse simple domains" $
            tryParse parseDomain domainSpecStr `shouldBe` Just domainSpecRes
        it "parses a domain D converted to a string as D" $
            tryParse parseDomain (writeDomain domainSpecRes) `shouldBe` Just domainSpecRes

    describe "Problem specification parser" $ do
        it "can parse simple problems" $ do
            tryParse parseProblem problemSpecStr `shouldBe` Just problemSpecRes


spec :: Spec
spec = testParsePredicateSpec

main :: IO ()
main = hspec spec