module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (fromList, insertWith, notMember, Map)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

toNucleotide :: Char -> Nucleotide
toNucleotide c
  | c == 'A' = A
  | c == 'C' = C
  | c == 'G' = G
  | otherwise = T

foo :: String -> Map Nucleotide Int -> Either String (Map Nucleotide Int)
foo [] m = Right m
foo (x:xs) m =
  if x `notElem` ['A','C','G','T'] || notMember nucleotide m then Left "Error"
  else foo xs (insertWith (+) nucleotide 1 m)
  where
      nucleotide = toNucleotide x

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foo xs (fromList[(A,0), (C,0), (G,0), (T, 0)])
