module Acronym (abbreviate) where

import Data.Char (isUpper, toUpper, isAlpha)

abbreviate :: String -> String
abbreviate "" = ""
abbreviate (f:s:xs) | isAlpha s && (not . isAlpha) f = toUpper s:abbreviate xs
                    | isUpper f && isUpper s = abbreviate (f:xs)
abbreviate (c:xs)   | isUpper c = c:abbreviate xs
                    | otherwise = abbreviate xs
