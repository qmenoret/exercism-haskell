module Atbash (decode, encode) where

import qualified Data.Map as M (fromList, lookup)
import Data.Char (toLower, isDigit, isAlpha)
import Data.Maybe

decodeMap = M.fromList $ zip (['a'..'z']++['0'..'9']) (reverse ['a'..'z'] ++ ['0'..'9'])
encodeMap = M.fromList $ zip (reverse ['a'..'z'] ++ ['0'..'9']) (['a'..'z']++['0'..'9'])

decode :: String -> String
decode = foldl transform []
    where
        transform acc x = if isNothing (find x)
            then acc
            else acc ++ [fromJust (find x)]
        find x = M.lookup (toLower x) decodeMap

encode :: String -> String
encode = tail . reccurE . filter (\x -> isDigit x || isAlpha x)

reccurE :: String -> String
reccurE "" = ""
reccurE xs = ' ':foldl transform [] (take 5 xs) ++ reccurE (drop 5 xs)
    where
        transform acc x = if isNothing (find x)
            then acc
            else acc ++ [fromJust (find x)]
        find x = M.lookup (toLower x) encodeMap
