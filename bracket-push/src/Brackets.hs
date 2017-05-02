module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = arePaired' xs ""

arePaired' :: String -> String -> Bool
arePaired' "" "" = True
arePaired' "" _  = False
arePaired' (x:xs) ys
    | x == '('  = arePaired' xs (x:ys)
    | x == ')'  = not (null ys) && head ys == '(' && arePaired' xs (tail ys)
    | x == '{'  = arePaired' xs (x:ys)
    | x == '}'  = not (null ys) && head ys == '{' && arePaired' xs (tail ys)
    | x == '['  = arePaired' xs (x:ys)
    | x == ']'  = not (null ys) && head ys == '[' && arePaired' xs (tail ys)
    | otherwise = arePaired' xs ys

