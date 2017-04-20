module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices 0 _  = [[]]
slices n xs = slices' n xs (l-n)
    where
        l = length xs

slices' :: Int -> String -> Int -> [[Int]]
slices' n xs i | i <  0     = []
               | i == 0     = [extract]
               | otherwise  =  extract : slices' n (tail xs) (i-1)
    where extract = (map digitToInt . take n) xs
