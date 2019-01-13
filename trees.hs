import Data.Function
import Data.Maybe
import Control.Monad

data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Eq, Show)

type Map k v = BinTree (k, v)

maxSumPath :: (Ord a, Num a) => BinTree a -> a
maxSumPath Empty = 0
maxSumPath (Node v t1 t2) = v + ((max `on` maxSumPath) t1 t2)

isEmpty :: BinTree a -> Bool
isEmpty Empty = True
isEmpty _ = False

isLeaf :: BinTree a -> Bool
isLeaf Empty = False
isLeaf (Node _ t1 t2) = isEmpty t1 && isEmpty t2

prune :: BinTree a -> BinTree a
prune Empty = Empty
prune t@(Node a t1 t2) = if isLeaf t
                           then Empty
                           else Node a (prune t1) (prune t2)

leaf :: a -> BinTree a
leaf x = Node x Empty Empty

bloom :: a -> BinTree a -> BinTree a
bloom _ Empty = Empty
bloom x t@(Node y t1 t2) = if isLeaf t
                             then Node y (leaf x) (leaf x)
                             else Node y (bloom x t1) (bloom x t2)

empty :: Map k v
empty = Empty

insert :: Ord k => k -> v -> Map k v -> Map k v
insert key val Empty = leaf (key, val)
insert key val (Node n@(k, v) t1 t2) = if k == key
                                           then Node (key, val) t1 t2
                                           else if key < k
                                                  then Node n (insert key val t1) t2
                                                  else Node n t1 (insert key val t2)
                                         
search :: Ord k => k -> Map k v -> Maybe v
search key Empty = Nothing
search key (Node (k, v) t1 t2) = if k == key
                                   then Just v
                                   else (mplus `on` (search key)) t1 t2

data Direction = LeftD | RightD

instance Show Direction where
    show LeftD = "L"
    show RightD = "R"

type BST a = BinTree a

bstPath :: Ord a => a -> BST a -> Maybe [Direction]
bstPath x t = path t
    where path Empty = Nothing
          path (Node y t1 t2) = if x == y
                             then Just []
                             else mplus (add LeftD t1) (add RightD t2)
          add d tree = do
                         p <- path tree
                         return $ d:p
                                    


exampleTree :: BinTree Int
exampleTree =
  Node 1 (Node 2 (leaf 3) Empty) (leaf 4)

exMap :: Map Int Int
exMap = insert 2 32 (insert 1 12 (insert 0 10 empty))


bst :: BST Int
bst = Node 2 (Node 0 (leaf (-1)) (leaf 1))  (leaf 3) 

main = do
         --print $ maxSumPath exampleTree
         --print $ prune exampleTree
         --print $ bloom 9 $ exampleTree
         --line <- getLine
         --let x = read line
         --print $ bloom x $ exampleTree
         --print exMap
         --print $ search 2 exMap
         --print $ search 3 exMap
         print $ bstPath 1 bst
         print $ bstPath 2 bst
         print $ bstPath 9 bst
