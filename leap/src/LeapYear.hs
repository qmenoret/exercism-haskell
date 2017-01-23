module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear x = (x `mod` 100 /= 0 || x `mod` 400 == 0) && x `mod` 4 == 0
