module RunLength (decode, encode) where

import Data.Char
import Data.Maybe


-- Helper type
type Count  = Int
type Occ    = (Count, Maybe Char)

nbOcc :: Occ -> Count
nbOcc (x, _) = x

charOcc :: Occ -> Maybe Char
charOcc (_, y) = y

realChar :: Occ -> Char
realChar = fromJust . charOcc

-- Encoding part
split :: [Occ] -> String -> [Occ]
split xs "" = xs
split (x:xs) (y:ys) | y == realChar x = split ((1 + nbOcc x, Just y):xs) ys
split xs     (y:ys)                   = split ((1, Just y):xs) ys

encodeOcc :: [Occ] -> String
encodeOcc [] = ""
encodeOcc (x:xs) = encodeOcc xs ++ count ++ [realChar x]
    where count = if nbOcc x == 1 then "" else (show . nbOcc) x

-- Decoding part
splitEncoded :: [Occ] -> String -> [Occ]
splitEncoded ys "" = ys
splitEncoded (y:ys) (x:xs) | isDigit x && (isNothing . charOcc) y = splitEncoded ((10 * nbOcc y + digitToInt x, Nothing):ys) xs
splitEncoded (y:ys) (x:xs) | isNothing (charOcc y)                = splitEncoded ((nbOcc y, Just x):ys) xs
splitEncoded ys     (x:xs) | isDigit x                            = splitEncoded ((digitToInt x, Nothing):ys) xs
splitEncoded ys     (x:xs)                                        = splitEncoded ((1, Just x):ys) xs

decodeOcc :: [Occ] -> String
decodeOcc [] = ""
decodeOcc (x:xs) = decodeOcc xs ++ replicate (nbOcc x) (realChar x)

-- Exports
decode :: String -> String
decode = decodeOcc . splitEncoded []

encode :: String -> String
encode = encodeOcc . split []

