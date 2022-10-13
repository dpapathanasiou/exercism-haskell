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

data BST a = EmptyBST | Node a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft EmptyBST = Nothing
bstLeft (Node _ left _) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight EmptyBST = Nothing
bstRight (Node _ _ right) = Just right

bstValue :: BST a -> Maybe a
bstValue EmptyBST = Nothing
bstValue (Node a _ _) = Just a

empty :: BST a
empty = EmptyBST

fromList :: Ord a => [a] -> BST a
fromList xs = foldr insert EmptyBST (reverse xs)

insert :: Ord a => a -> BST a -> BST a
insert x EmptyBST = singleton x
insert x (Node a left right)
  | x <= a    = Node a (insert x left) right
  | otherwise = Node a left (insert x right)

singleton :: a -> BST a
singleton x = Node x EmptyBST EmptyBST

toList :: BST a -> [a]
toList EmptyBST = []
toList (Node a left right) = toList left ++ [a] ++ toList right
