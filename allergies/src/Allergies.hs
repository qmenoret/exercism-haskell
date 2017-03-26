module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Enum)

allergies :: Int -> [Allergen]
allergies score = foldl (\acc x -> if isAllergicTo x score then x:acc else acc) [] (enumFrom Eggs)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = score .&. bit (fromEnum allergen) /= 0

