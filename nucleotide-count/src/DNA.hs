module DNA (count, nucleotideCounts) where

import Data.Map (Map, fromList, (!))
import Data.Either

count :: Char -> String -> Either String Int
count _ "" = Right 0
count a xs
    | badInput = Left "NOPE"
    | otherwise = count map
    where
        map = nucleotideCounts xs
        badInput = a `notElem` "ACGT" || isLeft map
        count (Right m) = Right (m ! a)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs
    | badInput = Left "Bad input detected, should only contain A,C,G,T"
    | otherwise = Right $ fromList [('A', count' 'A' xs)
                                   ,('C', count' 'C' xs)
                                   ,('G', count' 'G' xs)
                                   ,('T', count' 'T' xs)
                                   ]
    where
        count' a = foldl (\acc x -> if x == a then acc + 1 else acc) 0
        isBadChar c = c `notElem` "ACGT"
        badInput = foldl (\acc x -> acc || isBadChar x) False xs
