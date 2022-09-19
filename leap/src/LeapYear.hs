module LeapYear (isLeapYear) where

isEvenlyDivisible :: Integer -> Integer -> Bool
isEvenlyDivisible x y = rem x y == 0

isLeapYear :: Integer -> Bool
isLeapYear year = divFour && not divHundred || divFour && divHundred && divFourHundred
    where 
        divFour = isEvenlyDivisible year 4
        divHundred = isEvenlyDivisible year 100
        divFourHundred = isEvenlyDivisible year 400
