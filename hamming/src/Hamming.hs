module Hamming (distance) where

import Data.Maybe

distance :: String -> String -> Maybe Integer
distance xs ys | length xs /= length ys = Nothing
distance a b = (Just . count a) b
    where
        count "" "" = 0
        count (x:xs) (y:ys) = (diff x y +) $ count xs ys
        diff c d = if c == d then 0 else 1
