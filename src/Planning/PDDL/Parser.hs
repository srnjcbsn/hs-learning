module Planning.PDDL.Parser
    ( module Planning.PDDL.Parser
    , Text.ParserCombinators.Parsec.ParseError
    ) where

import           Control.Applicative           ((<*), (*>))
import           Control.Monad                 (liftM)
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Text.ParserCombinators.Parsec

import           Logic.Formula
import           Planning
import           Planning.PDDL

acceptableRequirements :: [String]
acceptableRequirements = [":typing", ":strips"]

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | Parses a string starting with a ';' character and ending with a newline.
-- Returns the string without the leading ';' character and the trailing newline.
comment :: Parser String
comment = char ';' >> manyTill anyChar newline

-- | Parses any number of comments surrounded by any amount of whitespace.
-- returns the list of parsed comments
comments :: Parser [String]
comments = spaces *> endBy comment spaces

parseRequirement :: Parser String
parseRequirement = choice $ map (try . string) acceptableRequirements

parseName :: Parser String
parseName = do
    first <- letter
    rest <- many (alphaNum <|> oneOf "-_")
    return $ first : rest

parseArgRef :: Parser Name
parseArgRef = char '?' >> parseName

typeSep :: Parser Char
typeSep = char '-'

-- many1Till :: Parser a -> Parser b -> Parser a
-- many1Till p q = notFollowedBy

typeString :: Parser Type
typeString = char '-' >> spaces >> parseName <* spaces

sameTypedList :: Parser a -> Parser [(a, Type)]
sameTypedList p = do
    ps <- many1 (p <* spaces)
    t <- option baseType typeString
    return $ map (flip (,) t) ps

typedList :: Parser a -> Parser [(a, Type)]
typedList p = liftM concat $ many (sameTypedList p)

parseArgument :: Parser Term
parseArgument =
        liftM TName parseName
    <|> liftM TVar  parseArgRef

groundedPredicate :: Parser GroundedPredicate
groundedPredicate = parens $ do
    gname <- parseName
    spaces
    params <- sepBy parseName spaces
    return $ Predicate gname params

-- | Parse an action applied to a list of arguments.
-- This is an alias for 'groundedNamed'
action :: Parser Action
action = parens $ do
    aname <- parseName
    spaces
    params <- sepBy parseName spaces
    return (aname, params)

parsePredicateSpec :: Parser PredicateSpec
parsePredicateSpec = do
    _ <- char '('
    pname <- parseName
    spaces
    params <- typedList parseArgRef -- sepBy parseArgRef space
    _ <- char ')'
    return $ Predicate pname params

parseFluent :: Parser FluentPredicate
parseFluent =
    parens $ do
        fname <- parseName
        spaces
        args <- parseArgument `sepBy` spaces
        return $ Predicate fname args

parseConjunction :: Parser a -> Parser [a]
parseConjunction f = parens $ string "and" >> spaces >> (f `sepBy` spaces)

parseNegation :: Parser a -> Parser a
parseNegation f = parens $ string "not" >> spaces >> f

-- parseConjunction :: (Show a, Ord a, Eq a)
--                  => Parser (Formula a)
--                  -> Parser (Formula a)
-- parseConjunction f =
--     parens $ liftM Con $ string "and" >> spaces >> f `sepBy` spaces
--
-- parseNegation :: (Show a, Ord a, Eq a) => Parser (Formula a) -> Parser (Formula a)
-- parseNegation f =
--     parens $ liftM Neg $ string "not" >> spaces >> f
--
-- parseFormula :: Parser (Formula Argument)
-- parseFormula =
--         try (parseConjunction parseFormula)
--     <|> try (parseNegation parseFormula)
--     <|> liftM Pred parseFluent

parseType :: Parser String
parseType = char '-' >> spaces >> parseName

-- groundedFormula :: Parser (Formula Name)
-- groundedFormula =
--         try (parseConjunction groundedFormula)
--     <|> try (parseNegation groundedFormula)
--     <|> liftM Pred groundedPredicate

parseLiteral :: Parser (LitPred Term)
parseLiteral = try (parens $ string "not" >> spaces >> parseFluent >>= return . Neg)
                <|> (parseFluent >>= return . Pos)

parseGoalDescription :: Parser GoalDesc
parseGoalDescription = 
        try (liftM GAnd $ parseConjunction parseGoalDescription)
    <|> try (liftM GNot $ parseNegation parseGoalDescription)
    <|> liftM GLit parseLiteral

parseEffect :: Parser Effect
parseEffect = 
        try (liftM EAnd (parseConjunction parseEffect))
    <|> liftM ELit parseLiteral

parseActionSpec :: Parser ActionSpec
parseActionSpec =
    parens $ do
        string ":action " >> spaces
        aname <- parseName
        spaces >> string ":parameters " >> spaces
        ts <- parens $ typedList parseArgRef
        --params <- parens $ typedList --(parseArgRef `sepBy` spaces)
        spaces
        string ":precondition " >> spaces
        precond <- parseGoalDescription
        spaces
        string ":effect " >> spaces
        eff <- parseEffect
        spaces
        return ActionSpec { asName      = aname
                          , asParas     = map fst ts
                          , asPrecond   = precond
                          , asEffect    = eff
                          , asConstants = []
                          , asTypes     = Map.fromList ts
                          }

parseDomain :: Parser PDDLDomain
parseDomain =
    parens $ do
        _ <- string "define "
        dname <- parens $ string "domain " >> parseName
        spaces
        _ <- parens $ string ":requirements " >> sepBy spaces parseRequirement
        spaces
        types <- parens $ string ":types " >> parseName `sepBy` spaces
        spaces
        consts <- parens $ string ":constants " >> parseName `sepBy` spaces
        spaces
        preds <- parens $ string ":predicates " >> parsePredicateSpec `sepBy` spaces
        spaces
        dactions <- parseActionSpec `sepEndBy1` spaces
        return PDDLDomain { dmName         = dname
                          , dmPredicates   = preds
                          , dmActionsSpecs = dactions
                          , dmConstants    = consts
                          , dmTypes        = types
                          }

parseProblem :: Parser PDDLProblem
parseProblem =
    parens $ do
        _ <- string "define "
        pname <- parens $ string "problem " >> parseName
        spaces
        dom <- parens $ string ":domain " >> parseName
        spaces
        objs <- parens $ string ":objects " >> typedList parseName
        spaces
        ini <- parens $ string ":init " >> groundedPredicate `sepBy` comments
        spaces
        g <- parens $ string ":goal " >> parseGoalDescription
        _ <- comments
        return PDDLProblem { probName = pname
                           , probDomain = dom
                           , probObjs = map fst objs
                           , probState = Set.fromList ini
                           , probGoal = g
                           , probTypes = Map.fromList objs
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
