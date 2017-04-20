module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!))

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix =   [ (a,b) 
                        | a <- [0..x], b <- [0..y]
                        , matrix ! (a,b) == maxRow a
                        , matrix ! (a,b) == minCol b]
    where
        (min, max) = bounds matrix
        (x, y) = max
        maxRow v = maximum [ matrix ! (v, a) | a <- [0..y] ]
        minCol v = minimum [ matrix ! (b, v) | b <- [0..x] ]
