module Planning.PDDL.Parser
    ( module Planning.PDDL.Parser
    , Text.ParserCombinators.Parsec.ParseError
    ) where

import           Control.Applicative           ((*>))
import           Control.Monad                 (liftM)
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Text.ParserCombinators.Parsec

import           Logic.Formula
import           Planning                      ()
import           Planning.PDDL

acceptableRequirements :: [String]
acceptableRequirements = [":strips", ":typing"]

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
parseRequirement = choice $ map string acceptableRequirements

parseName :: Parser String
parseName = do
    first <- letter
    rest <- many (alphaNum <|> oneOf "-_")
    return $ first : rest

parseArgRef :: Parser Name
parseArgRef = char '?' >> parseName

parseArgument :: Parser Argument
parseArgument =
        liftM Const parseName
    <|> liftM Ref   parseArgRef
--
-- groundedNamed :: Parser GroundedPredicate
-- groundedNamed = parens $ do
--     name <- parseName
--     spaces
--     params <- sepBy parseName spaces
--     return $ Predicate name params

groundedPredicate :: Parser GroundedPredicate
groundedPredicate = parens $ do
    name <- parseName
    spaces
    params <- sepBy parseName spaces
    return $ Predicate name params

-- | Parse an action applied to a list of arguments.
-- This is an alias for 'groundedNamed'
action :: Parser Action
action = parens $ do
    name <- parseName
    spaces
    params <- sepBy parseName spaces
    return (name, params)

parsePredicateSpec :: Parser PredicateSpec
parsePredicateSpec = do
    _ <- char '('
    name <- parseName
    spaces
    params <- sepBy parseArgRef space
    _ <- char ')'
    return $ Predicate name params

parseFluent :: Parser FluentPredicate
parseFluent =
    parens $ do
        name <- parseName
        spaces
        args <- parseArgument `sepBy` spaces
        return $ Predicate name args

parseConjunction :: (Show a, Ord a, Eq a)
                 => Parser (Formula a)
                 -> Parser (Formula a)
parseConjunction f =
    parens $ string "and"
             >> spaces
             >> f `sepBy` spaces
             >>= return . Con

parseNegation :: (Show a, Ord a, Eq a) => Parser (Formula a) -> Parser (Formula a)
parseNegation f =
    parens $ string "not"
             >> spaces
             >> f
             >>= return . Neg

parseFormula :: Parser (Formula Argument)
parseFormula =
        try (parseConjunction parseFormula)
    <|> try (parseNegation parseFormula)
    <|> (parseFluent >>= return . Pred)

parseType :: Parser String
parseType = char '-' >> spaces >> parseName

groundedFormula :: Parser (Formula Name)
groundedFormula =
        try (parseConjunction groundedFormula)
    <|> try (parseNegation groundedFormula)
    <|> (groundedPredicate >>= return . Pred)

parseActionSpec :: Parser ActionSpec
parseActionSpec =
    parens $ do
        _ <- string ":action "
        spaces
        name <- parseName
        spaces
        _ <- string ":parameters "
        spaces
        params <- parens (parseArgRef `sepBy` spaces)
        spaces
        _ <- string ":precondition "
        spaces
        precond <- parseFormula
        spaces
        _ <- string ":effect "
        spaces
        eff <- parseFormula
        spaces
        return ActionSpec { asName    = name
                          , asParas   = params
                          , asPrecond = precond
                          , asEffect  = eff
                          , asConstants = []
                          , asTypes = Map.empty
                          }

parseDomain :: Parser PDDLDomain
parseDomain =
    parens $ do
        _ <- string "define "
        name <- parens $ string "domain " >> parseName
        spaces
        _ <- parens $ string ":requirements " >> many parseRequirement
        spaces
        types <- parens $ string ":types " >> parseName `sepBy` spaces
        spaces
        consts <- parens $ string ":constants " >> parseName `sepBy` spaces
        spaces
        preds <- parens $ string ":predicates " >> parsePredicateSpec `sepBy` spaces
        spaces
        actions <- parseActionSpec `sepEndBy1` spaces
        return PDDLDomain { dmName         = name
                          , dmPredicates   = preds
                          , dmActionsSpecs = actions
                          , dmConstants    = consts
                          , dmTypes        = types
                          }

parseProblem :: Parser PDDLProblem
parseProblem =
    parens $ do
        _ <- string "define "
        name <- parens $ string "problem " >> parseName
        spaces
        dom <- parens $ string ":domain " >> parseName
        spaces
        objs <- parens $ string ":objects " >> parseName `sepBy` spaces
        spaces
        ini <- parens $ string ":init " >> groundedPredicate `sepBy` comments
        spaces
        g <- parens $ string ":goal " >> groundedFormula
        _ <- comments
        return PDDLProblem { probName = name
                           , probDomain = dom
                           , probObjs = objs
                           , probState = Set.fromList ini
                           , probGoal = g
                           , probTypes = Map.empty
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
