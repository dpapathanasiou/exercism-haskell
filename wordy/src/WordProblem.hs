module WordProblem (answer) where

{-
  Based on these tutorials:
    https://jakewheat.github.io/intro_to_parsing/
    https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
-}

import Text.ParserCombinators.Parsec

data MathExpr = Number Integer
  | Add MathExpr MathExpr
  | Subtract MathExpr MathExpr
  | Multiply MathExpr MathExpr
  | Divide MathExpr MathExpr
  | Unsupported String
  deriving (Eq, Show)

parseNumber :: Parser MathExpr
parseNumber = do
    n <- many1 (digit <|> char '-')
    return $ Number (read n)

parseOperator :: Parser MathExpr
parseOperator = do
    x  <- parseNumber
    op <- many1 (letter <|> space)
    y  <- parseNumber
    return $ case op of
     " plus "          -> Add x y
     " minus "         -> Subtract x y
     " multiplied by " -> Multiply x y
     " divided by "    -> Divide x y
     _ -> Unsupported op

parseMathExpr :: Parser MathExpr
parseMathExpr = try parseOperator <|> parseNumber

combineMathNum :: (Integer -> Integer -> Integer) -> Maybe Integer -> Maybe Integer -> Maybe Integer
combineMathNum _ Nothing _ = Nothing
combineMathNum _ _ Nothing = Nothing
combineMathNum f (Just x) (Just y) = Just (f x y)

computeMathExpr :: MathExpr -> Maybe Integer
computeMathExpr (Unsupported _) = Nothing
computeMathExpr (Number n) = Just n
computeMathExpr (Add x y) = combineMathNum (+) (computeMathExpr x) (computeMathExpr y)
computeMathExpr (Subtract x y) = combineMathNum (-) (computeMathExpr x) (computeMathExpr y)
computeMathExpr (Multiply x y) = combineMathNum (*) (computeMathExpr x) (computeMathExpr y)
computeMathExpr (Divide x y) = combineMathNum div (computeMathExpr x) (computeMathExpr y)

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