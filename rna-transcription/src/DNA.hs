module DNA (toRNA) where

rna :: Char -> Char
rna 'C' = 'G'
rna 'G' = 'C'
rna 'T' = 'A'
rna 'A' = 'U'
rna _ = '_'


toRNA :: String -> Maybe String
toRNA xs = result
    where 
        translation = foldr (\x acc -> rna x : acc) "" xs
        result = if '_' `notElem` translation then Just translation else Nothing
