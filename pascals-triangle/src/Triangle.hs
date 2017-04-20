module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x | x <= 0 = []
       | otherwise = r' x [1]

r' :: Int -> [Integer] -> [[Integer]]
r' 1 previous = [previous]
r' x previous = previous: next
    where
        current = 1: calc previous
        next = r' (x-1) current

calc :: [Integer] -> [Integer]
calc [x]        = [x]
calc (x:y:xs)   = (x+y): calc (y:xs)
