module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA = mapM rna
    where 
        rna :: Char -> Maybe Char
        rna 'C' = Just 'G'
        rna 'G' = Just 'C'
        rna 'T' = Just 'A'
        rna 'A' = Just 'U'
        rna _ = Nothing
