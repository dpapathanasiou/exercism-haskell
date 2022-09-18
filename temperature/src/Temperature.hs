module Temperature (tempToC, tempToF) where

{- Implement the function `tempToC` to convert
`  Fahrenheit to Celsius                    -}

integerToFloat :: Integer -> Float
integerToFloat x = fromIntegral x :: Float

tempToC :: Integer -> Float
tempToC temp = (integerToFloat (temp - 32)) / 1.8


{- Implement the function `tempToF` to convert
`  Celsius to Fahrenheit                    -}


tempToF :: Float -> Integer
tempToF temp = (ceiling ((temp * 1.8) + 32))
