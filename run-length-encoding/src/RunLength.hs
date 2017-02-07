module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode "" = ""
decode xs = replicate nb ch ++ (decode . drop 1 . snd) ss
    where ss = span isDigit xs
          nb = max 1 $ (read . ('0':) . fst) ss
          ch = (head . snd) ss

encode :: String -> String
encode "" = ""
encode xs = dispCount ++ char : encode ys
    where (same, ys) = span (== char) xs
          count = length same
          dispCount = if count == 1 then "" else show count
          char = head xs

