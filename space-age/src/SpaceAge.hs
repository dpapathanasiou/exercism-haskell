module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

norm :: Float
norm = 31557600.0

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = seconds / (norm * 0.2408467)
ageOn Venus seconds = seconds / (norm * 0.61519726)
ageOn Earth seconds = seconds / norm
ageOn Mars seconds = seconds / (norm * 1.8808158)
ageOn Jupiter seconds = seconds / (norm * 11.862615)
ageOn Saturn seconds = seconds / (norm * 29.447498)
ageOn Uranus seconds = seconds / (norm * 84.016846)
ageOn Neptune seconds = seconds / (norm * 164.79132)
