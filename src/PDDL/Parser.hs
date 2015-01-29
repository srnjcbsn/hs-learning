module PDDL.Parser where

import Text.ParserCombinators.Parsec
import PDDL.Type
import Control.Monad (liftM2)

parsePredicate :: Parser Predicate
parsePredicate = undefined --between (char "(") (char ")") parseParameters >>

parseIdentifier :: Parser Char -> Parser String
parseIdentifier p =
    do first <- p
       rest  <- many alphaNum
       return (first : rest)

parseName :: Parser String
parseName = parseIdentifier lower

parseConstant = parseIdentifier upper >>= return . Const

parseArgRef = char '?' >> parseName >>= return . Ref

parseArgument :: Parser Argument
parseArgument = parseConstant <|> parseArgRef
    --     (parseConstant >>= return . Const)
    -- <|> (parseArgRef   >>= return . Ref)

parsePredicateSpec :: Parser PredicateSpec
parsePredicateSpec =
    do char '('
       name <- parseName
       space
       params <- sepBy parseName space
       char ')'
       return (name, params)

parseFormula :: Parser Formula
parseFormula = undefined --(sepBy (char " ") char)


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

p = parse parsePredicateSpec "unknown"
