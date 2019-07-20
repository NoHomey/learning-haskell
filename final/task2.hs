allEqual :: [[Int]] -> [Int -> Int] -> [Int]
allEqual ls fs = case build $ zipWith (\l f -> zip l $ map f l) ls fs of
                     [] -> []
                     ((k, xs):_) -> xs
    where build [z] = map (\(x, fx) -> (fx, [x])) z
          build (h:t) = [(k, x:xs) | (k, xs) <- build t, (x, fx) <- h, fx == k]
                        
main = print $ allEqual [[1,2], [3,4], [5,6]] [(+1), id, (8-)]