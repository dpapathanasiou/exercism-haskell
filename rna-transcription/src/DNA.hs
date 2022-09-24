module DNA (toRNA) where

dnaToRna :: Char -> Maybe Char
dnaToRna dna 
  | dna == 'G' = Just 'C'
  | dna == 'C' = Just 'G'
  | dna == 'T' = Just 'A'
  | dna == 'A' = Just 'U'
  | otherwise = Nothing

foo :: String -> String -> Either Char String
foo [] a = Right a
foo (x:xs) a = case dnaToRna x of
    Just rna -> foo xs (a ++ [rna])
    Nothing  -> Left x

toRNA :: String -> Either Char String
toRNA text = foo text ""