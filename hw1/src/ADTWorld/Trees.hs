module ADTWorld.Trees
       ( Tree (..)
       , isLeaf
       , isEmpty
       , size
       , findKey
       , insertKey
       , fromList
       , toList
       ) where

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving(Eq, Show)

isLeaf :: Tree a -> Bool
isLeaf Leaf{} = True
isLeaf _      = False

isEmpty :: Tree a -> Bool
isEmpty = isLeaf

size :: Tree a -> Int
size Leaf         = 0
size (Node _ l r) = 1 + size l + size r

findKey :: Ord a => Tree a -> a -> Bool
findKey Leaf _           = False
findKey (Node x l r) key
  | key == x  = True
  | otherwise = findKey l key || findKey r key

insertKey :: Ord a => Tree a -> a -> Tree a
insertKey Leaf key              = Node key Leaf Leaf
insertKey node@(Node x l r) key
  | key < x  = Node x (insertKey l key) r
  | key == x = node
  | key > x  = Node x l (insertKey r key)

fromList :: Ord a => [a] -> Tree a
fromList []     = Leaf
fromList (x:xs) = Node x (fromList $ filter (< x) xs) (fromList $ filter (> x) xs)

toList :: Tree a -> [a]
toList Leaf         = []
toList (Node x l r) = toList l ++ [x] ++ toList r
