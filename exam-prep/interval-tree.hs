data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)

mergeIntervals :: (Ord a) => (a, a) -> (a, a) -> (a, a)
mergeIntervals (a, b) (i, j) = (min a i, max b j)

childrenInterval :: (Ord a) => BinTree (a, a) -> BinTree (a, a) -> (a, a)
childrenInterval Empty (Node i _ _) = i
childrenInterval (Node i _ _) Empty = i
childrenInterval (Node x _ _) (Node y _ _) = mergeIntervals x y

intervalTree :: (Ord a, Num a) => BinTree a -> BinTree (a, a)
intervalTree Empty = Empty
intervalTree (Node x Empty Empty) = Node (x, x) Empty Empty
intervalTree (Node x left right) = let left' = intervalTree left
                                       right' = intervalTree right
                                       c = childrenInterval left' right'
                                   in Node (mergeIntervals (x, x) c) left' right'

leaf :: a -> BinTree a
leaf x = Node x Empty Empty

main = print $ intervalTree $ Node 5 (Node 3 (Node 1 Empty $ leaf 2) $ leaf 4) $ leaf 6