Data.Function (on)

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = foldl (\(pxs, npxs) x -> if p x then (x:pxs, npxs) else (pxs, x:npxs)) ([], [])

histogram :: (Eq a) => [a] -> [(a, Int)]
histogram [] = []
histogram x:xs = let (xeq, nxeq) = partition (x ==) xs
                 in (x, (length xeq) + 1):histogram xs

predExtremal :: (a -> a -> Bool) -> [a] -> a
predExtremal ext (x:xs) = foldl (\extream curr -> if ext curr extream then curr else extream) x xs

mostFrequent :: (Eq a) => [[a]] -> a
mostFrequent l = let processed = process l
                     shortest = fst $ predExtremal ((<) `on` snd) processed
                     
                 in any (\e -> every (elem e) processed) shortest
                 where process l = map (\list -> (list, length list)) . onlyMaxOccurs . histogram l
                                   where onlyMaxOccurs hist = let maxOccur = snd $ predExtremal ((>) `on` snd) hist
                                                              in filter ((maxOccur ==) . snd) hist

main :: IO()
main = do
       print $ mostFrequent [[1,1,3,2],[1,1,5],[1,5],[1,1,1,3]]
       print $ mostFrequent [[1,1,3,2],[1,5,5],[1,5],[1,1,1,3]]