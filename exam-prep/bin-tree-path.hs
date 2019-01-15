import Control.Monad

data BinTree = Empty | Node Int BinTree BinTree

sameAsCode :: BinTree -> Int
sameAsCode t = case helper t 1 of
                   Nothing -> 0
                   Just x -> x
    where helper Empty _ = Nothing
          helper (Node x left right) p = if x == p
                                           then Just x
                                           else let p' = 2 * p
                                                in (helper left p') `mplus` (helper right $ p' + 1)

tree = Node 5 (Node 3 Empty $ Node 2 Empty Empty) $ Node 4 (Node 6 Empty Empty) Empty

main = print $ sameAsCode tree