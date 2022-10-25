module WordProblem (answer) where

{-
  Based on these tutorials:
    https://jakewheat.github.io/intro_to_parsing/
    https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
-}

import Text.ParserCombinators.Parsec

data MathExpr = Number Integer | Unsupported String
  deriving (Eq, Show)

generateMathExpr :: String -> Integer -> Integer -> MathExpr
generateMathExpr operator x y = case operator of
    " plus "          -> Number (x + y)
    " minus "         -> Number (x - y)
    " multiplied by " -> Number (x * y)
    " divided by "    -> Number (x `div` y)
    _ -> Unsupported operator

parseNumber :: Parser MathExpr
parseNumber = do
    n <- many1 (digit <|> char '-')
    return $ Number (read n)

parseOperator :: Parser MathExpr
parseOperator = do
    (Number x) <- parseNumber
    operator   <- many1 (letter <|> space)
    (Number y) <- parseNumber
    return $ generateMathExpr operator x y

parseNextOperator :: Parser MathExpr
parseNextOperator = do
    (Number x) <- parseOperator
    operator   <- many1 (letter <|> space)
    (Number y) <- parseNumber
    return $ generateMathExpr operator x y

parseMathExpr :: Parser MathExpr
parseMathExpr = try parseNextOperator <|> try parseOperator <|> try parseNumber

computeMathExpr :: MathExpr -> Maybe Integer
computeMathExpr (Unsupported _) = Nothing
computeMathExpr (Number n) = Just n

parseMathStmt :: Parser MathExpr
parseMathStmt = do
    _ <- string "What is "
    e <- parseMathExpr
    _ <- char '?'
    return e

answer :: String -> Maybe Integer
answer problem = case parse parseMathStmt "math" problem of
    Left _  -> Nothing
    Right x -> computeMathExpr x