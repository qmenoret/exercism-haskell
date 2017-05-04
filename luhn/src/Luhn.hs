module Luhn (addends, checkDigit, checksum, create, isValid) where

import Data.Char (digitToInt, intToDigit)
import Data.List (find)
import Data.Maybe (fromMaybe)

addends :: Integer -> [Integer]
addends n = xs
    where
        i = toInteger.digitToInt
        xs = reverse . doubleMap . reverse . show $ n
        doubleMap []       = [] 
        doubleMap [x]      = [i x] 
        doubleMap (x:y:xs) = i x : double y : doubleMap xs
        double x = r
            where
                t = i x *2
                r = if t > 9 then t-9 else t

checkDigit :: Integer -> Integer
checkDigit n = n `rem` 10

checksum :: Integer -> Integer
checksum n = (`rem` 10) . sum $ addends n

create :: Integer -> Integer
create n = if isValid n then n else valid
    where
        next = n * 10
        els = map (+next) [0..9]
        valid = fromMaybe (create next) $ find isValid els

isValid :: Integer -> Bool
isValid n = checksum n == 0
