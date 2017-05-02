module CryptoSquare (encode) where

import Data.Char (toLower, isAlphaNum)
import Data.List.Split (chunksOf)
import Data.List (transpose)

encode :: String -> String
encode xs = unwords transposed
    where
        filtered = (filter isAlphaNum . map toLower) xs
        l = length filtered
        s = (floor . sqrt . fromIntegral) l
        x = if s*s == l then s else s+1
        transposed = (transpose . chunksOf x) filtered
