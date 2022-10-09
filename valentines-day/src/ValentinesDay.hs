{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module ValentinesDay where

data Approval = Yes | No | Maybe deriving (Eq, Show)

data Cuisine = Korean | Turkish deriving (Eq, Show)

data Genre = Crime | Horror | Romance | Thriller deriving (Eq, Show)

data Activity = BoardGame | Chill | Movie Genre | Restaurant Cuisine | Walk Int deriving (Eq, Show)

{-
Finally, you're ready to rate your partner's ideas. This is how you feel about your partner's idea:

    Playing a board game: no.
    Chill out: no.
    Watch a movie: yes if it is a romantic movie; otherwise, no.
    Go to a restaurant: yes if the cuisine is Korean, maybe if it is Turkish.
    Take a walk: yes if the walk is less than three kilometers; maybe if it is between three and five kilometers (inclusive); otherwise, no.
-}

rateActivity :: Activity -> Approval
rateActivity activity = case activity of
    BoardGame -> No
    Chill -> No
    Movie genre -> if genre == Romance then Yes else No
    Restaurant cuisine -> if cuisine == Korean then Yes else if cuisine == Turkish then Maybe else No
    Walk km -> if km < 3 then Yes else if km <= 5 then Maybe else No
    _ -> error "Invalid activity"
