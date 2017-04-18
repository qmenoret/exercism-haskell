module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import qualified Data.Vector as V (Vector, fromList)
import qualified Data.List   as L (transpose)
import Control.Arrow     ((&&&))
import Data.Maybe (isNothing, fromJust)

newtype Matrix a = Matrix [[a]]
    deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix [])     = 0
cols (Matrix (v:vs)) = length v

column :: Int -> Matrix a -> V.Vector a
column x (Matrix vs) = V.fromList $ map (!! x) vs

flatten :: Matrix a -> V.Vector a
flatten (Matrix xs) = V.fromList $ concat xs

fromList :: [[a]] -> Matrix a
fromList = Matrix

fromString :: Read a => String -> Matrix a
fromString xs = Matrix rows
    where
        rows = map transform $ lines xs
        transform [] = []
        transform xs =
            case reads xs :: (Read a => [(a, String)]) of
                []      -> transform $ tail xs
                [(s,e)] -> s : transform e

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape d (Matrix xs) = Matrix $ make d (concat xs)
    where
        make (0,y) xs = []
        make (x,y) xs = start: make (x-1,y) end
            where
                (start,end) = splitAt y xs

row :: Int -> Matrix a -> V.Vector a
row 0 (Matrix (v:vs)) = V.fromList v
row x (Matrix (v:vs)) = row (x-1) (Matrix vs)

rows :: Matrix a -> Int
rows (Matrix vs) = length vs

shape :: Matrix a -> (Int, Int)
shape = rows &&& cols

transpose :: Matrix a -> Matrix a
transpose (Matrix vs) = Matrix $ L.transpose vs

