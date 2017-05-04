module Queens (boardString, canAttack) where

import Data.List (intersperse)

defaultBoard = replicate 8 $ replicate 8 '_'

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString w b = unlines $ map (intersperse ' ') updatedBoard
    where
        updatedBoard = place b 'B' $ place w 'W' defaultBoard

place :: Maybe (Int, Int) -> Char -> [String] -> [String]
place Nothing _ b      = b
place (Just (x,y)) c b = newList x b
    where
        newList 0 (x:xs) = newRow x:newList (-1) xs
        newList n (x:xs) = x: newList (n-1) xs
        newList _ []     = []
        newRow r = take y r ++ c:drop (y+1) r

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1,y1) (x2,y2) =    x1 == x2
                            || y1 == y2
                            || a == b
    where
        a = if x1 > x2
            then abs x1-x2
            else abs x2-x1
        b = if y1 > y2
            then abs y1-y2
            else abs y2-y1
