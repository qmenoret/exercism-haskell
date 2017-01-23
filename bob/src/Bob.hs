module Bob (responseFor) where

import Data.Char

responseForLastChar :: Char -> String
responseForLastChar '?' = "Sure."
responseForLastChar _ = "Whatever."

isUpperString :: String -> Bool
isUpperString "" = False
isUpperString xs = foldr (\x acc -> acc && isUpper x) True xs

isSpaceOnly :: String -> Bool
isSpaceOnly = foldr (\x acc -> acc && isSpace x) True

responseFor :: String -> String
responseFor x | isSpaceOnly x = "Fine. Be that way!"
responseFor x | (isUpperString . filter isAlpha) x = "Whoa, chill out!"
responseFor x = (responseForLastChar . last . filter (not . isSpace)) x
