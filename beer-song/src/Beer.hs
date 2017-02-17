module Beer (song) where

beers 0 = "no more bottles"
beers 1 = "1 bottle"
beers n = show n ++ " bottles"

down 1 = "it"
down _ = "one"

getVerse :: Int -> String
getVerse 0 = "No more bottles of beer on the wall, no more bottles of beer.\n\
       \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
getVerse n = beers n ++ " of beer on the wall, " ++ beers n ++ " of beer.\n\
       \Take " ++ down n ++ " down and pass it around, " ++ beers (n-1) ++ " of beer on the wall.\n\
       \\n"

song :: String
song = foldl (\acc x -> getVerse x ++ acc) "" [0..99]
