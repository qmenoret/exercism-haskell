module Anagram (anagramsFor) where

import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter isAna
    where
        lstr = toLowerString xs
        sorted = sort lstr
        toLowerString = map toLower
        isAna x = ((== sorted) . sort) lx && (lx /= lstr) 
            where
                lx = toLowerString x
