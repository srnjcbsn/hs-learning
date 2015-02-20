module PDDL.Parser
    ( module PDDL.Parser
    , Text.ParserCombinators.Parsec.ParseError
    ) where

import           Control.Applicative           ((*>), (<*))
import           Control.Monad                 (liftM2)
import qualified Data.Set                      as Set
import           PDDL
import           Text.ParserCombinators.Parsec

acceptableRequirements :: [String]
acceptableRequirements = [":strips"]

parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') p

-- | Parses a string starting with a ';' character and ending with a newline.
-- Returns the string without the leading ';' character and the trailing newline.
comment :: Parser String
comment = char ';' >> manyTill anyChar newline

-- | Parses any number of comments surrounded by any amount of whitespace.
-- returns the list of parsed comments
comments :: Parser [String]
comments = spaces *> endBy comment spaces

parseRequirement :: Parser String
parseRequirement = choice $ map string acceptableRequirements

parseName :: Parser String
parseName = do
    first <- letter
    rest <- many (alphaNum <|> oneOf "-_")
    return $ first : rest

parseArgRef = char '?' >> parseName

parseArgument :: Parser Argument
parseArgument =
        (parseName   >>= return . Const)
    <|> (parseArgRef >>= return . Ref)

groundedNamed :: Parser (Name, [String])
groundedNamed = parens $ do
    name <- parseName
    spaces
    params <- sepBy parseName spaces
    return (name, params)

-- | Parse a grounded predicate. This is an alias for 'groundedNamed'
groundedPredicate :: Parser GroundedPredicate
groundedPredicate = groundedNamed

-- | Parse an action applied to a list of arguments.
-- This is an alias for 'groundedNamed'
action :: Parser Action
action = groundedNamed

parsePredicateSpec :: Parser PredicateSpec
parsePredicateSpec = do
    char '('
    name <- parseName
    spaces
    params <- sepBy parseArgRef space
    char ')'
    return (name, params)

parseFluent :: Parser FluentPredicate
parseFluent =
    parens $ do
        name <- parseName
        spaces
        args <- parseArgument `sepBy` spaces
        return (name, args)

parseConjunction :: Parser Formula
parseConjunction =
    parens $ string "and"
             >> spaces
             >> parseFormula `sepBy` spaces
             >>= return . Con

parseNegation :: Parser Formula
parseNegation =
    parens $ string "not"
             >> spaces
             >> parseFormula
             >>= return . Neg

parseFormula :: Parser Formula
parseFormula =
        try parseConjunction
    <|> try parseNegation
    <|> (parseFluent >>= return . Predicate)


parseActionSpec :: Parser ActionSpec
parseActionSpec =
    parens $ do
        string ":action "
        spaces
        name <- parseName
        spaces
        string ":parameters "
        spaces
        params <- parens (parseArgRef `sepBy` spaces)
        spaces
        string ":precondition "
        spaces
        precond <- parseFormula
        spaces
        string ":effect "
        spaces
        eff <- parseFormula
        spaces
        return ActionSpec { asName = name
                          , asParas = params
                          , asPrecond = precond
                          , asEffect = eff
                          }

parseDomain :: Parser Domain
parseDomain =
    parens $ do
        string "define "
        name <- parens $ string "domain " >> parseName
        spaces
        parens $ string ":requirements " >> many parseRequirement
        spaces
        consts <- parens $ string ":constants " >> parseName `sepBy` spaces
        spaces
        preds <- parens $ string ":predicates " >> parsePredicateSpec `sepBy` spaces
        spaces
        actions <- parseActionSpec `sepEndBy1` spaces
        return Domain { dmName         = name
                      , dmPredicates   = preds
                      , dmActionsSpecs = actions
                      , dmConstants    = consts
                      }

parseProblem :: Parser Problem
parseProblem =
    parens $ do
        string "define "
        name <- parens $ string "problem " >> parseName
        spaces
        dom <- parens $ string ":domain " >> parseName
        spaces
        objs <- parens $ string ":objects " >> parseName `sepBy` spaces
        spaces
        ini <- parens $ string ":init " >> groundedPredicate `sepBy` comments
        spaces
        g <- parens $ string ":goal " >> parseFormula
        _ <- comments
        return Problem { probName = name
                       , probDomain = dom
                       , probObjs = objs
                       , probState = Set.fromList ini
                       , probGoal = g
                       }

plan :: Parser Plan
plan = action `endBy` comments

doParse :: Parser a -> String -> Either ParseError a
doParse ps = parse ps "pddl"

tryParse :: Parser a -> String -> Maybe a
tryParse ps str =
    case parse ps "" str of
        Left _  -> Nothing
        Right a -> Just a

-- actionSpecStr = unlines [ "(:action act"
--                         , ":parameters (?a)"
--                         , ":precondition (p ?a)"
--                         , ":effect (not (p ?a)) )"
--                         ]
--
-- domainSpecStr =
--     unlines [ "(define (domain test)"
--             , "(:requirements :strips)"
--             , "(:constants (A B))"
--             , "(:predicates (a ?x ?y))"
--             , actionSpecStr
--             , ")"
--             ]

-- problemSpecStr =
--     unlines [ "(define (problem prob)"
--             , "(:domain dom)"
--             , "(:objects x y)"
--             , "(:init (test1 x y))"
--             , "(:goal (test2 x y)) )"
--             ]
-- actionSpecRes = ActionSpec { asName    = "act"
--                            , asParas   = ["a"]
--                            , asPrecond = Predicate ("p", [Ref "a"])
--                            , asEffect  = Neg (Predicate ("p", [Ref "a"]))
--                            }
-- domainSpecRes =
--     Domain { dmName         = "test"
--            , dmPredicates   = [("a", ["x", "y"])]
--            , dmActionsSpecs = [actionSpecRes]
--            , dmConstants    = ["A", "B"]
--            }

p = parse parseDomain "unknown"
