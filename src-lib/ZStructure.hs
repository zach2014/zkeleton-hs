module ZStructure (BinTree(..), treeFromList, treeFromList') where

-- | infinite structure 

data BinTree a = Empty | Node a (BinTree a ) (BinTree a)  deriving (Show, Eq)

treeFromList ::  Ord a => [a] -> BinTree a
treeFromList = foldr insert Empty where
    insert x Empty = Node x Empty Empty
    insert x (Node y l r) | x < y = Node y (insert x l) r
                          | otherwise = Node y l (insert x r)

treeFromList' :: Ord a => [a] -> BinTree a
treeFromList' [] = Empty
treeFromList' (x:xs) = Node x (treeFromList' (filter (< x) xs)) (treeFromList' (filter (> x) xs))