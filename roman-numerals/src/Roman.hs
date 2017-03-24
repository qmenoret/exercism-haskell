module Roman (numerals) where

rnTen :: Int -> Char -> Char -> Char -> String
rnTen i one five ten
    | i < 4 = replicate i one
    | i == 4 = [one,five]
    | i == 5 = [five]
    | i < 9 = five : replicate (i-5) one
    | i == 9 = [one,ten]

rn :: Int -> String
rn x
    | x < 10    = rnTen x 'I' 'V' 'X'
    | x < 100   = rnTen (quot x 10)   'X' 'L' 'C' ++ rn (x `mod` 10)
    | x < 1000  = rnTen (quot x 100)  'C' 'D' 'M' ++ rn (x `mod` 100)
    | otherwise = rnTen (quot x 1000) 'M' '?' '?' ++ rn (x `mod` 1000)

numerals :: Integer -> Maybe String
numerals = Just . rn . fromIntegral
