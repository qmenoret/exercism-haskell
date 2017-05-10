module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)

data CustomSet a = MySet { value    :: a
                         , next     :: CustomSet a
                         , len      :: Int
                         }
                 | Empty
    deriving (Show)

instance Eq a => Eq (CustomSet a) where
    Empty == Empty  = True
    Empty == _      = False
    _     == Empty  = False
    setA  == setB   = isSubsetOf setA setB && size setA == size setB

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x Empty = Empty
delete x MySet { value=a, next=n }
    | a == x = n
    | otherwise = MySet { value=a, next=nextSet, len=1+size nextSet }
    where
        nextSet = delete x n

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference Empty setB  = Empty
difference setA  Empty = setA
difference setA MySet{value=a, next=n} = difference (delete a setA) n

empty :: CustomSet a
empty = Empty

fromList :: Eq a => [a] -> CustomSet a
fromList = foldr insert Empty

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x Empty = MySet { value=x, next=Empty, len=1 }
insert x MySet { value=a, next=n }
    | x == a    = MySet { value=a, next=n,       len=1+size n }
    | otherwise = MySet { value=a, next=nextSet, len=1+size nextSet }
    where
        nextSet = insert x n

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection Empty  _       = Empty
intersection _      Empty   = Empty
intersection MySet{value=a, next=n} set2
    | member a set2 = MySet { value=a, next=nextSet, len=1+size nextSet }
    | otherwise     = nextSet
        where
            nextSet = intersection n set2

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom Empty     _      = True
isDisjointFrom _        Empty   = True
isDisjointFrom MySet{value=a, next=n} set2 = not (member a set2) && isDisjointFrom n set2

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf Empty _ = True
isSubsetOf _ Empty = False
isSubsetOf MySet { value=a, next=n } set2 = member a set2 && isSubsetOf n set2

member :: Eq a => a -> CustomSet a -> Bool
member x Empty = False
member x MySet { value=a, next=n }
    | x == a    = True
    | otherwise = member x n

null :: CustomSet a -> Bool
null Empty = True
null _     = False

size :: CustomSet a -> Int
size Empty = 0
size MySet { len=n } = n

toList :: CustomSet a -> [a]
toList Empty = []
toList MySet { value=a, next=n } = a: toList n

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union Empty Empty = Empty
union Empty setB  = setB
union setA  Empty = setA
union MySet { value=a, next=n } setB = insert a $ union n setB


