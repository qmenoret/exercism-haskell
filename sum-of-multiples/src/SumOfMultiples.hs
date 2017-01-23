module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer 
sumOfMultiples a b = sum $ possibilities a b
    where
        possibilities :: [Integer] -> Integer -> [Integer]
        possibilities xs l = [a | a <- [1..l-1], foldr (\x acc -> acc || a `mod` x == 0) False xs]
