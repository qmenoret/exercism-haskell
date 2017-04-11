module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = foldl present True ['a'..'z']
    where
        present acc c = acc && (c `elem` upText)
        upText = map toLower text
