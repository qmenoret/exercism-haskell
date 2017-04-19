module House (rhyme) where

import Data.List (intercalate)

type Verse  = (String, String)

phrases =   [ ("that lay in the house that Jack built.\n",          "This is the house that Jack built.\n")
            , ("that ate the malt\n",                               "This is the malt\n")
            , ("that killed the rat\n",                             "This is the rat\n")
            , ("that worried the cat\n",                            "This is the cat\n")
            , ("that tossed the dog\n",                             "This is the dog\n")
            , ("that milked the cow with the crumpled horn\n",      "This is the cow with the crumpled horn\n")
            , ("that kissed the maiden all forlorn\n",              "This is the maiden all forlorn\n")
            , ("that married the man all tattered and torn\n",      "This is the man all tattered and torn\n")
            , ("that woke the priest all shaven and shorn\n",       "This is the priest all shaven and shorn\n")
            , ("that kept the rooster that crowed in the morn\n",   "This is the rooster that crowed in the morn\n")
            , ("that belonged to the farmer sowing his corn\n",     "This is the farmer sowing his corn\n")
            , ("",                                                  "This is the horse and the hound and the horn\n")
            ]

rhyme :: String
rhyme = (intercalate "\n" . map concatVerse . reverse . foldl makeVerse []) phrases
    where
        makeVerse :: [[Verse]] -> Verse -> [[Verse]]
        makeVerse []     v = [[v]]
        makeVerse (x:xs) v = (v:x):(x:xs)

        concatVerse :: [Verse] -> String
        concatVerse []     = ""
        concatVerse (v:vs) = snd v ++ concatMap fst vs

