module GuessingGame (reply) where

correct :: Int 
correct = 42

reply :: Int -> String
reply guess
        | guess == correct = "Correct"
        | guess == (correct - 1) || guess == (correct + 1) = "So close"
        | guess > correct = "Too high"
        | otherwise = "Too low"