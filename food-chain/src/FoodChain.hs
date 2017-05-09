module FoodChain (song) where

import Data.List (intercalate)

parts = [ ("fly",       "", "")
        , ("spider",    "It wriggled and jiggled and tickled inside her.\n",        "She swallowed the spider to catch the fly.\n")
        , ("bird",      "How absurd to swallow a bird!\n",                          "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n")
        , ("cat",       "Imagine that, to swallow a cat!\n",                        "She swallowed the cat to catch the bird.\n")
        , ("dog",       "What a hog, to swallow a dog!\n",                          "She swallowed the dog to catch the cat.\n")
        , ("goat",      "Just opened her throat and swallowed a goat!\n",           "She swallowed the goat to catch the dog.\n")
        , ("cow",       "I don't know how she swallowed a cow!\n",                  "She swallowed the cow to catch the goat.\n")
        ]

firstLine animal = "I know an old lady who swallowed a " ++ animal ++ ".\n"
lastLine         = "I don't know why she swallowed the fly. Perhaps she'll die.\n"

nextVerse :: [String] -> (String, String, String) -> [String]
nextVerse []           (a, b, c) = firstLine a : [lastLine]
nextVerse (p:r:evious) (a, b, c)
    | null evious = firstLine a : b : c : r : evious
    | otherwise   = firstLine a : b : c : evious

reccur :: [(String, String, String)] -> [String] -> [String]
reccur [] _ = []
reccur (x:xs) prev = concat current : reccur xs current
    where current = nextVerse prev x

song :: String
song = intercalate "\n" $ reccur parts [] ++ ["I know an old lady who swallowed a horse.\nShe's dead, of course!\n"]

