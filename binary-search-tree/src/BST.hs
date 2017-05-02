module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = BST { left  :: BST a
                 , right :: BST a
                 , value :: a
                 }
            | Empty
    deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft BST {left=Empty}  = Nothing
bstLeft BST {left=l}      = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight BST {right=Empty}  = Nothing
bstRight BST {right=r}      = Just r

bstValue :: BST a -> Maybe a
bstValue Empty          = Nothing
bstValue BST {value=x}  = Just x

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList []     = Empty
fromList (x:xs) = foldl (flip insert) (singleton x) xs

insert :: Ord a => a -> BST a -> BST a
insert x BST {right=r, left=l, value=v}
    | v >= x    = BST{left=insert x l,   right=r,            value=v}
    | otherwise = BST{left=l,            right=insert x r,   value=v}
insert x empty = singleton x 

singleton :: a -> BST a
singleton x = BST {left=empty, right=empty, value=x}

toList :: BST a -> [a]
toList Empty = []
toList BST{left=l, right=r, value=v} = toList l ++ (v:toList r)
