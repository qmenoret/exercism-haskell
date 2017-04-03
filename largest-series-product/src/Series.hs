module Series (largestProduct) where

import Data.Char
import Data.Maybe

largestProduct :: Int -> String -> Maybe Integer
largestProduct size xs  | length xs >= size && onlyDigit && size >= 0 = reccur its size xs
                        | otherwise         = Nothing
    where
        onlyDigit = all isDigit xs
        l = length xs
        its = l - size + 1

reccur :: Int -> Int -> String -> Maybe Integer
reccur 0 _ _ = Just 0
reccur its size xs = if p > next then p else next
    where
        p = Just . fromIntegral . product $ map digitToInt (take size xs)
        next = reccur (its - 1 ) size $ tail xs
