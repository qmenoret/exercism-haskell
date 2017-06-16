module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (toLower, isLower)
import System.Random

caesarDecode :: String -> String -> String
caesarDecode a b = map r $ zipLoop a b
    where
        r (x, y) = (toEnum $ fromEnum 'a' + diff x y) :: Char
        diff x y = (26 + toPos y - toPos x) `rem` 26

caesarEncode :: String -> String -> String
caesarEncode a b = map r $ zipLoop a b
    where
        r (x, y) = (toEnum $ fromEnum 'a' + diff x y) :: Char
        diff x y = (toPos x + toPos y) `rem` 26

toPos :: Char -> Int
toPos a = (fromEnum . toLower) a - 97

zipLoop :: [a] -> [b] -> [(a, b)]
zipLoop short long = r short long
    where
        r _ [] = []
        r [] a = r short a
        r (x:xs) (y:ys) = (x,y):r xs ys

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    key <- generateStr 1000
    return (key, caesarEncode key text)

generateStr :: Int -> IO String
generateStr 0   = return ""
generateStr len = (:) <$> generateChar <*> generateStr (len - 1)
    where
        generateChar :: IO Char
        generateChar = toEnum <$> getStdRandom (randomR (97, 122))
