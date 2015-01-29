module PDDL.Parser where

import Text.ParserCombinators.Parsec
import PDDL.Type
import Control.Monad (liftM2)

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


parseFluent :: Parser FluentPredicate
parseFluent = do
    char '('
    name <- parseName
    spaces
    args <- sepBy parseArgument space
    char ')'
    return (name, args)

parseConjunction = do
    string "(and"
    spaces
    fluents <- parseFormula `sepBy` spaces
    char ')'
    return $ Con fluents

parseNegation = do
    string "(not"
    spaces
    f <- parseFormula
    char ')'
    return $ Neg f

parseFormula :: Parser Formula
parseFormula =
        try parseConjunction
    <|> try parseNegation
    <|> (parseFluent >>= return . Predicate)

parseDomain :: String -> Parser Domain
parseDomain = undefined

parseProblem :: String -> Parser Problem
parseProblem = undefined

parseSmth :: Parser [Int]
parseSmth = sepBy parseDigit (char ',')

parseDigit :: Parser Int
parseDigit = many digit >>= return . read

-- doParse :: Parser a -> String -> Either ParserError a
doParse ps str = parse ps "" str

tryParse :: Parser a -> String -> Maybe a
tryParse ps str =
    case parse ps "" str of
        Left _  -> Nothing
        Right a -> Just a

p = parse parseFormula "unknown"
