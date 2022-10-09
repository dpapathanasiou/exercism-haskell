module SecretHandshake (handshake) where
import Data.Char (intToDigit)

toBinary :: Int -> String -> Int
toBinary i acc
  | q == 0 = read bin :: Int
  | otherwise = toBinary q bin
  where (q, r) = i `quotRem` 2
        bin = intToDigit r : acc

encode :: Int -> [String] -> [String]
encode n acc
  | n - 1000 >= 0 = encode (n - 1000) ("jump" : acc)
  | n - 100 >= 0  = encode (n - 100)  ("close your eyes" : acc)
  | n - 10 >= 0   = encode (n - 10)   ("double blink" : acc)
  | n - 1 >= 0    = encode (n - 1)    ("wink" : acc)
  | otherwise = acc

handshake :: Int -> [String]
handshake n
  | bin >= 10000 = reverse (encode (bin - 10000) [])
  | otherwise = encode bin []
  where bin = toBinary n ""