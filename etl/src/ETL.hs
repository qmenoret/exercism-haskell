module ETL (transform) where

import Data.List (foldl)
import Data.Map (Map, foldWithKey, insert, empty)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform = foldWithKey insertString (empty:: Map Char a)
    where
        insertString cv cs newMap = foldl (\m x -> insert (toLower x) cv m) newMap cs
