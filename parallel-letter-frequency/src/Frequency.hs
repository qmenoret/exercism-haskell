module Frequency (frequency) where

import Data.Map (Map, empty, insertWith, unionsWith)
import qualified Data.Text as T (Text, foldl)
import Data.Char (toLower, isAlpha)
import Control.Parallel (par, pseq)

frequency :: Int -> [T.Text] -> Map Char Int
frequency nWorkers texts = unionsWith (+) a
    where
        a = map frequencyOne texts

frequencyOne :: T.Text -> Map Char Int
frequencyOne = T.foldl compute empty
    where
        compute map char
            | isAlpha char = insertWith func (toLower char) 1 map
            | otherwise = map
        func new old = old+new
