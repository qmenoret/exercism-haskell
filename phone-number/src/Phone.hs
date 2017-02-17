module Phone (number) where

import Data.Char
import Data.Maybe

removeChars :: String -> String -> Maybe String
removeChars "" _ = Just ""
removeChars (x:xs) chars | x `elem` chars = next
                         | (not . isDigit) x || isNothing next = Nothing
                         | otherwise = Just $ x:fromJust next
    where next = removeChars xs chars

number :: String -> Maybe String
number xs   | badInput = Nothing
            | l == 10 && r == '1' = Just esult
            | l == 9   = Just $ r:esult
            | otherwise = Nothing
    where 
        result = removeChars xs "() -."
        (r:esult) = fromJust result
        l = length esult
        badInput = isNothing result

