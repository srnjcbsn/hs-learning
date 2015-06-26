module Planning.PDDL.ParserSpec (main, spec) where

import           Data.Char
import qualified Data.Map                 as Map
import           Data.Maybe               (isNothing)
import qualified Data.Set                 as Set

import           Test.Hspec
import           Test.QuickCheck

import           Logic.Formula
import           Planning.PDDL
import           Planning.PDDL.Parser
import           Planning.PDDL.Serializer


testParsePredicateSpec :: Spec
testParsePredicateSpec =
    let p :: (Variable -> Term) -> Variable -> Predicate Term
        p f x = Predicate "p" [f x]

        predStrA :: String
        predStrA = "(a ?x Y)"

        predResA :: Predicate Term
        predResA = Predicate "a" [TVar "x", TName "Y"]

        actionSpecStr :: String
        actionSpecStr = unlines [ "(:action act"
                                , ":parameters (?a)"
                                , ":precondition (p ?a)"
                                , ":effect (not (p ?a)) )"
                                ]

        actionSpecRes :: ActionSpec
        actionSpecRes = ActionSpec { asName      = "act"
                                   , asParas     = ["a"]
                                   , asPrecond   = gPos $ p TVar "a"
                                   , asEffect    = eNeg $ p TVar "a"
                                   , asConstants = []
                                   , asTypes     = Map.singleton "a" baseType
                                   }
        domainSpecStr :: String
        domainSpecStr =
            unlines [ "(define (domain test)"
                    , "(:requirements :strips)"
                    , "(:types )"
                    , "(:constants A B)"
                    , "(:predicates (a ?x ?y))"
                    , actionSpecStr
                    , ")"
                    ]

        domainSpecRes :: PDDLDomain
        domainSpecRes =
            PDDLDomain { dmName         = "test"
                       , dmPredicates   = [Predicate "a" [ ("x", baseType)
                                                         , ("y", baseType)]
                                          ]
                       , dmActionsSpecs = [actionSpecRes]
                       , dmConstants    = ["A", "B"]
                       , dmTypes        = []
                       }

        problemSpecStr :: String
        problemSpecStr =
            unlines [ "(define (problem prob)"
                    , "(:domain dom)"
                    , "(:objects x y)"
                    , "(:init (test1 x y))"
                    , "(:goal (test2 x y)) )"
                    ]

        problemSpecRes :: PDDLProblem
        problemSpecRes =
            PDDLProblem { probName = "prob"
                        , probObjs = ["x", "y"]
                        , probDomain = "dom"
                        , probState = Set.singleton $ Predicate "test1" ["x", "y"]
                        , probGoal = GLit $ Pos $ Predicate "test2" [TName "x", TName "y"]
                        , probTypes = Map.fromList [("x", baseType), ("y", baseType)]
                        }
      in do
    describe "Identifier parser" $ do
        it "can contain '-', '_', and numbers" $ do
            tryParse parseName "a-" `shouldBe` Just "a-"
            tryParse parseName "a_" `shouldBe` Just "a_"
            tryParse parseName "a1" `shouldBe` Just "a1"
            tryParse parseName "a1A-_" `shouldBe` Just "a1A-_"
        it "can only start with a letter" $ property $
            \c -> let parsed = tryParse parseName (c : "a")
                  in  if isLetter c then parsed == Just (c : "a")
                      else isNothing parsed

    describe "Predicate specification parser" $ do
        it "can parse predicate specifications" $
            tryParse parsePredicateSpec "(a ?x ?y)" `shouldBe`
                Just (Predicate "a" [("x", baseType), ("y", baseType)])
        it "can parse predicate specs with no parameters" $ do
            tryParse parsePredicateSpec "(a)"  `shouldBe` Just (Predicate "a" [])
            tryParse parsePredicateSpec "(a )" `shouldBe` Just (Predicate "a" [])

    describe "Argument reference parser" $ do
        it "requires a '?' prefix" $
            tryParse parseArgRef "a" `shouldBe` Nothing
        it "throws away the prefix" $
            tryParse parseArgRef "?a" `shouldBe` Just "a"
        it "requires a letter after the '?'" $
            tryParse parseArgRef "?" `shouldBe` Nothing

    describe "Argument parser" $ do
        it "parses a '?'-prefixed string as a reference" $
            tryParse parseArgument "?a" `shouldBe` Just (TVar "a")
        it "parses a string starting with a capital letter as a constant" $
            tryParse parseArgument "A" `shouldBe` Just (TName "A")

    describe "Fluent parser" $
        it "can parse fluents with 'Argument's" $
            tryParse parseFluent predStrA `shouldBe` Just predResA

    -- describe "Formula parser" $ do
    --     it "can parse fluents" $
    --         tryParse parseFormula predStrA `shouldBe` Just (Pred predResA)
    --     it "can parse negated formulae" $
    --         tryParse parseFormula ("(not" ++ predStrA ++ ")")
    --             `shouldBe` Just (Neg (Pred predResA))
    --     it "can parse conjunctions" $
    --         tryParse parseFormula ("(and " ++ predStrA ++ " " ++ predStrB ++ ")")
    --             `shouldBe` Just (Con [Pred predResA, Pred predResB])

    describe "Typed list parser" $ do
        it "can parse an untyped list as objects" $
            tryParse (typedList parseName) "a b c" `shouldBe`
                Just [ ("a", baseType)
                     , ("b", baseType)
                     , ("c", baseType)
                     ]

        it "can parse a list mixed types" $
            tryParse (typedList parseName) "a b - t1 c - t2 d" `shouldBe`
                Just [ ("a", "t1")
                     , ("b", "t1")
                     , ("c", "t2")
                     , ("d", baseType)
                     ]

    describe "Plan parser" $
        it "can parse simple plans (as produced by fast-downward)" $
            let planStr = unlines [ "(push-h b0x0 b0x0 b1x0 b2x0)"
                                  , "(push-h b2x1 b2x2 boxat1x0 boxat2x1)"
                                  , "; cost = 2 (unit cost)"
                                  ]
                expectedPlan = [ ("push-h", ["b0x0", "b0x0", "b1x0", "b2x0"])
                               , ("push-h", ["b2x1", "b2x2", "boxat1x0", "boxat2x1"])
                               ]
            in tryParse plan planStr `shouldBe` Just expectedPlan

    describe "Action specification parser" $
        it "can parse basic action specifications" $
            tryParse parseActionSpec actionSpecStr `shouldBe` Just actionSpecRes

    describe "Domain definition parser" $ do
        it "can parse simple domains" $
            tryParse parseDomain domainSpecStr `shouldBe` Just domainSpecRes
        it "parses a domain D converted to a string as D" $
            tryParse parseDomain (writeDomain domainSpecRes) `shouldBe` Just domainSpecRes

    describe "Problem specification parser" $
        it "can parse simple problems" $
            tryParse parseProblem problemSpecStr `shouldBe` Just problemSpecRes


spec :: Spec
spec = testParsePredicateSpec

main :: IO ()
main = hspec spec
