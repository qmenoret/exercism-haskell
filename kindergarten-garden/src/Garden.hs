module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, fromList, (!))
import Data.List (sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

children = [
        "Alice", "Bob", "Charlie", "David",
        "Eve", "Fred", "Ginny", "Harriet",
        "Ileana", "Joseph", "Kincaid", "Larry"]

p :: Char -> Plant
p 'C' = Clover
p 'G' = Grass
p 'R' = Radishes
p 'V' = Violets

defaultGarden :: String -> Map String [Plant]
defaultGarden = garden children

garden :: [String] -> String -> Map String [Plant]
garden childList plants = fromList $ assignPlants (sort childList) (line1,line2)
    where
        (line1, n:line2) = span (/= '\n') plants
        assignPlants _ ([], []) = []
        assignPlants (child:cs) (a:b:l1, c:d:l2) = (child, [p a,p b,p c,p d]) : assignPlants cs (l1,l2)

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants = flip (!)
