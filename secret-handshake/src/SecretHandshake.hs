module SecretHandshake (handshake) where

import Data.Bits 

-- Keep reverse last to be the last applied
ops = [ (8, ("jump":)),
        (4, ("close your eyes":)),
        (2, ("double blink":)),
        (1, ("wink":)),
        (16, reverse)]

handshake :: Int -> [String]
handshake n = result
    where
        result = foldl apply [] ops
        apply xs bf | n .&. fst bf == 0 = xs
                          | otherwise   = snd bf xs
