module RunLength (decode, encode) where

import Data.Char

decode :: String -> String

decode "" = ""
decode xs = replicate nb ch ++ (decode . drop 1) s
    where (f,s) = span isDigit xs
          nb = max 1 $ (read . ('0':)) f
          ch = head s

encode :: String -> String
encode "" = ""
encode xs = dispCount ++ char : encode ys
    where (same, ys) = span (== char) xs
          count = length same
          dispCount = if count == 1 then "" else show count
          char = head xs

