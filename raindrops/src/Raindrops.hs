module Raindrops (convert) where

convert :: Int -> String
convert x = result $ foldl (\acc l -> acc ++ match l) "" list
    where
        result "" = show x
        result a = a
        match couple | x `mod` fst couple == 0 = snd couple
                     | otherwise = ""
        list =
            [(3,    "Pling")
            ,(5,    "Plang")
            ,(7,    "Plong")]
