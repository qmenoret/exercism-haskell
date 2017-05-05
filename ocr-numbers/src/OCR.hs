module OCR (convert) where

import Data.Maybe (fromMaybe)
import Data.List (find, transpose, intercalate)
import Data.List.Split (chunksOf)

zero =  [ " _ "
        , "| |"
        , "|_|"
        , "   "
        ]
one =   [ "   "
        , "  |"
        , "  |"
        , "   "
        ]
two =   [ " _ "
        , " _|"
        , "|_ "
        , "   "
        ]
three = [ " _ "
        , " _|"
        , " _|"
        , "   "
        ]
four =  [ "   "
        , "|_|"
        , "  |"
        , "   "
        ]
five =  [ " _ "
        , "|_ "
        , " _|"
        , "   "
        ]
six =   [ " _ "
        , "|_ "
        , "|_|"
        , "   "
        ]
seven = [ " _ "
        , "  |"
        , "  |"
        , "   "
        ]
eight = [ " _ "
        , "|_|"
        , "|_|"
        , "   "
        ]
nine =  [ " _ "
        , "|_|"
        , " _|"
        , "   "
        ]

nums = zip [zero,one,two,three,four,five,six,seven,eight,nine] ['0'..'9']

convert :: String -> String
convert str = intercalate "," ls
    where
        ls = map c . chunksOf 4 . lines $ str
        c = map convertOne . splitStrings

splitStrings :: [String] -> [[String]]
splitStrings = transpose . map (chunksOf 3)

convertOne :: [String] -> Char
convertOne xs = snd $ fromMaybe (zero, '?') candidate
    where
        candidate = find (\e -> xs == fst e) nums
