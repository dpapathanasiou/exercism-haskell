module DNA (toRNA) where

dnaToRna :: Char -> Either Char Char
dnaToRna 'G' = Right 'C'
dnaToRna 'C' = Right 'G'
dnaToRna 'T' = Right 'A'
dnaToRna 'A' = Right 'U'
dnaToRna  x  = Left x

foo :: String -> String -> Either Char String
foo [] a = Right a
foo (x:xs) a = case dnaToRna x of
    Right rna -> foo xs (a ++ [rna])
    Left  err -> Left err

toRNA :: String -> Either Char String
toRNA text = foo text ""