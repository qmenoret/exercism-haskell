module Base (rebase) where

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits 
    | inputBase < 2 || outputBase < 2 = Nothing
    | otherwise = do
    o <- fromBase inputBase inputDigits
    toBase outputBase o

fromBase :: Integral a => a -> [a] -> Maybe a
fromBase b = foldl makeBase (Just 0)
    where
        makeBase acc x
            | x < 0 || x >= b = Nothing
            | otherwise      = (+ x) . (* b) <$> acc

toBase :: Integral a => a -> a -> Maybe [a]
toBase b 0 = Just []
toBase b n 
    | n < 0     = Nothing
    | otherwise = (++ [n `mod` b]) <$> toBase b (n `quot` b)

