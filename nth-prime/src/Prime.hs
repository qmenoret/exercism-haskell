module Prime (nth) where

import Data.List ((!!))

nth :: Int -> Maybe Integer
nth n 
    | n < 1 = Nothing
    | otherwise = Just $ primes !! (n-1)

primes = 2 : sieve primes [3..]
  where
    sieve (p:ps) xs = let (h,t) = span (< p*p) xs
                      in h ++ sieve ps [x | x <- t, rem x p > 0]
