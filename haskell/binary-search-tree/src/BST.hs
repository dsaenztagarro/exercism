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

data BST a = Node a (BST a) (BST a) | Empty deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Node _ Empty _) = Nothing
bstLeft (Node _ l _)     = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight (Node _ _ Empty) = Nothing
bstRight (Node _ _ r)     = Just r

bstValue :: BST a -> Maybe a
bstValue Empty        = Nothing
bstValue (Node a _ _) = Just a

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList [] = Empty
fromList xs = foldl (flip insert) empty xs

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node a l r) | x <= a = Node a (insert x l) r
                      | otherwise = Node a l (insert x r)

singleton :: a -> BST a
singleton x = Node x Empty Empty

toList :: BST a -> [a]
toList Empty        = []
toList (Node a l r) = concat [toList l, [a], toList r]
