module PrimeFactors (primeFactors) where

import Data.Maybe (fromJust)
import Data.List  (find)

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = e : primeFactors (quot n e)
  where
      e = fromJust $ find (\x -> n `mod` x == 0) primes

primes = 2 : sieve primes [3..]
  where
    sieve (p:ps) xs = let (h,t) = span (< p*p) xs
                      in h ++ sieve ps [x | x <- t, rem x p > 0]
