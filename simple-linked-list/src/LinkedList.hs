module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Duo a (LinkedList a) | Nil

datum :: LinkedList a -> a
datum (Duo x _) = x

fromList :: [a] -> LinkedList a
fromList = foldr Duo Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Duo

next :: LinkedList a -> LinkedList a
next (Duo _ n) = n

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = inter Nil
    where
        inter newList (Duo x next) = inter (Duo x newList) next
        inter newList Nil = newList

toList :: LinkedList a -> [a]
toList Nil = []
toList (Duo x l) = x:toList l
