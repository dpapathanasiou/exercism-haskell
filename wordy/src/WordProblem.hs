module WordProblem (answer) where

{-
  Based on these tutorials:
    https://jakewheat.github.io/intro_to_parsing/
    https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
-}

import Text.ParserCombinators.Parsec

data MathExpr = Number Integer | Unsupported String
  deriving (Eq, Show)

parseNumber :: Parser MathExpr
parseNumber = do
    n <- many1 (digit <|> char '-')
    return $ Number (read n)

parseOperator :: Parser MathExpr
parseOperator = do
    (Number x) <- parseNumber
    operator   <- many1 (letter <|> space)
    (Number y) <- parseNumber
    return $ case operator of
     " plus "          -> Number (x + y)
     " minus "         -> Number (x - y)
     " multiplied by " -> Number (x * y)
     " divided by "    -> Number (x `div` y)
     _ -> Unsupported operator

parseMathExpr :: Parser MathExpr
parseMathExpr = try parseOperator <|> parseNumber

computeMathExpr :: MathExpr -> Maybe Integer
computeMathExpr (Unsupported _) = Nothing
computeMathExpr (Number n) = Just n

parseMathStmt :: Parser MathExpr
parseMathStmt = do
    _ <- string "What is "
    e <- parseMathExpr
    _ <- char '?'
    return e

-- for debugging
readExpr :: String -> String
readExpr problem = case parse parseMathStmt "math" problem of
    Left e  -> show e
    Right x -> show x

answer :: String -> Maybe Integer
answer problem = case parse parseMathStmt "math" problem of
    Left _  -> Nothing
    Right x -> computeMathExpr x