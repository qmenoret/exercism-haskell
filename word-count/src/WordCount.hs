module WordCount (wordCount) where

import Data.List (group, sort)
import Data.Char (isAlphaNum, toLower)
import Data.List.Split (wordsBy)

wordCount :: String -> [(String, Int)]
wordCount = map g . group . sort . map cleanup . wordsBy (\x -> (not.isAlphaNum) x && x /= '\'')
    where
        g (x:xs) = (x, length xs + 1)
        cleanup ('\'':ord)  = map toLower $ init ord
        cleanup word        = map toLower word
