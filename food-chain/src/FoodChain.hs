module FoodChain (song) where

swallowedWhat :: String -> String
swallowedWhat x = "I know an old lady who swallowed a " ++ x ++ "."

andThen :: String -> String
andThen x
  | x == "spider" = "It wriggled and jiggled and tickled inside her."
  | x == "bird"   = "How absurd to swallow a bird!"
  | x == "cat"    = "Imagine that, to swallow a cat!"
  | x == "dog"    = "What a hog, to swallow a dog!"
  | x == "goat"   = "Just opened her throat and swallowed a goat!"
  | x == "cow"    = "I don't know how she swallowed a cow!"
  | x == "horse"  = "She's dead, of course!"
  | otherwise     = "I don't know why she swallowed the fly. Perhaps she'll die."

swallowedBecause :: String -> String -> String
swallowedBecause x y = "She swallowed the " ++ x ++ " to catch the " ++ y ++ z ++ "."
  where z = if y == "spider" then " that wriggled and jiggled and tickled inside her" else ""

stanza :: [String] -> [String] -> [String]
stanza    []   a = a
stanza (x:xs) [] = if null xs then stanza xs [swallowedWhat x, andThen x] else stanza xs [swallowedWhat x, andThen x, swallowedBecause x (head xs)]
stanza (x:xs)  a = if null xs then stanza xs (a ++ [andThen x])           else stanza xs (a ++ [swallowedBecause x (head xs)])

song :: String
song = unlines (stanza ["fly"] [] ++ [""] 
  ++ stanza ["spider", "fly"] [] ++ [""] 
  ++ stanza ["bird", "spider", "fly"] [] ++ [""] 
  ++ stanza ["cat", "bird", "spider", "fly"] [] ++ [""] 
  ++ stanza ["dog", "cat", "bird", "spider", "fly"] [] ++ [""] 
  ++ stanza ["goat", "dog", "cat", "bird", "spider", "fly"] [] ++ [""] 
  ++ stanza ["cow", "goat", "dog", "cat", "bird", "spider", "fly"] [] ++ [""]
  ++ stanza ["horse"] []
  )
