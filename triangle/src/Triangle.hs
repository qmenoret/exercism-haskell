module Triangle (TriangleType(..), triangleType) where

import Data.List

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType a b c  | illegal sorted = Illegal
                    | equi = Equilateral 
                    | iso = Isosceles
                    | otherwise = Scalene
    where
        sorted = sort [a,b,c]
        cEq = (length . group) sorted
        iso = cEq == 2
        equi = cEq == 1 
        illegal [x,y,z] = z > x+y || (equi && a == 0)
