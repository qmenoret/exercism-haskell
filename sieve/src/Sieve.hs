module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n = put [2..n]
    where
        put :: [Integer] -> [Integer]
        put []     = []
        put [x]    = [x]
        put (x:xs) = x: put (filter (\y -> y `rem` x /= 0) xs)
