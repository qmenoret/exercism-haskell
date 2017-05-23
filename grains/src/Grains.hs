module Grains (square, total) where

import Data.Maybe

square :: Integer -> Maybe Integer
square x | x < 1 || x > 64 = Nothing
square n = Just (2^(n-1))

total :: Integer
total = (sum . mapMaybe square) [1..64]
-- total = ((* 2) . fromJust . square) 64 - 1
-- total = foldl (\acc x -> ((acc +) . fromJust . square) x) 0 [1..64]

