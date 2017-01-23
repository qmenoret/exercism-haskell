module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference l = squareOfSums l - sumOfSquares l

squareOfSums :: Integral a => a -> a
squareOfSums l = ((^2) . sum) [1..l]

sumOfSquares :: Integral a => a -> a
sumOfSquares l = foldl1 (\acc x -> acc + (x^2)) [0..l]
