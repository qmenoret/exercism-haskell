module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List (sort)

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) =  isPy (a, b, c)
                        || isPy (a, c, b)
                        || isPy (c, b, a)

isPy :: (Int, Int, Int) -> Bool
isPy (a, b, c) = a^2 + b^2 == c^2

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = (a,b,c)

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minFactor maxFactor = sort [(a,b,c) | c <- [minFactor..maxFactor], b <- [minFactor..c], a <- [minFactor..b], isPy (a,b,c)]
