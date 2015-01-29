module PDDL.Parser where

import Text.ParserCombinators.Parsec
import PDDL.Type
import Control.Monad (liftM2)

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
    parens $ do
        string "and"
        spaces
        fluents <- parseFormula `sepBy` spaces
        return $ Con fluents

parseNegation =
    parens $ do
        string "not"
        spaces
        f <- parseFormula
        return $ Neg f

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

-- actionSpecStr = unlines [ "(:action act"
--                         , ":parameters (?a)"
--                         , ":precondition (p ?a)"
--                         , ":effect (not (p ?a)) )"
--                         ]

p = parse parseActionSpec "unknown"
