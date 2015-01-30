module PDDL.Parser where

import           Control.Monad                 (liftM2)
import           PDDL.Type
import           Text.ParserCombinators.Parsec
import qualified Data.Set as Set

parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') p

eol :: Parser Char
eol = char '\n'

parseIdentifier :: Parser Char -> Parser String
parseIdentifier p =
    do first <- p
       rest  <- many alphaNum
       return (first : rest)

parseName :: Parser String
parseName = parseIdentifier lower

parseConstant = parseIdentifier upper

parseArgRef = char '?' >> parseName

parseArgument :: Parser Argument
parseArgument = -- parseConstant <|> parseArgRef
        (parseConstant >>= return . Const)
    <|> (parseArgRef   >>= return . Ref)

parsePredicateSpec :: Parser PredicateSpec
parsePredicateSpec = do
    char '('
    name <- parseName
    spaces
    params <- sepBy parseArgRef space
    char ')'
    return (name, params)

parseFluent =
    parens $ do
        name <- parseName
        spaces
        args <- parseArgument `sepBy` spaces
        return (name, args)

parseConjunction =
    parens $ string "and"
             >> spaces
             >> parseFormula `sepBy` spaces
             >>= return . Con

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
        name <- parseName
        spaces
        string ":parameters "
        params <- parens (parseArgRef `sepBy` spaces)
        spaces
        string ":precondition "
        precond <- parseFormula
        spaces
        string ":effect "
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
        consts <- parens $ string ":constants " >> parens (parseConstant `sepBy` spaces)
        spaces
        preds <- parens $ string ":predicates " >> parsePredicateSpec `sepBy` spaces
        spaces
        actions <- parseActionSpec `sepEndBy1` spaces
        return Domain { dmPredicates   = preds
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
        ini <- parens $ string ":init " >> parsePredicateSpec `sepBy` spaces
        spaces
        g <- parens $ string ":goal " >> parsePredicateSpec `sepEndBy` spaces
        return Problem { probName = name
                       , probInitialState = Set.fromList ini
                       , probGoalState = Set.fromList g
                       }

-- doParse :: Parser a -> String -> Either ParserError a
doParse ps str = parse ps "" str

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
--     unlines [ "(define (domain dom)"
--             , "(:constants (A B))"
--             , "(:predicates (a ?x ?y))"
--             , actionSpecStr
--             , ")"
--             ]

p = parse parseDomain "unknown"
