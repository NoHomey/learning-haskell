aProg :: (Eq a, Num a) => [[a]] -> [a -> a] -> [a]
aProg ls fs = case helper $ zip ls fs of
                  [] -> []
                  (x:_) -> map fst x
    where helper [(ns, f)] = map (\n -> [(n, f n)]) ns
          helper ((l, f):ps) = case helper ps of
                                   [] -> []
                                   as -> buildMore l f as
          buildMore ns f as = [pas | n <- ns, a <- as, let p = (n, f n), let pas = p:a, isAProg pas]
          isAProg (_:_:[]) = True
          isAProg ((_, x):(_, y):(_, z):_) = (y - x) == (z - y)

main = print $ aProg [[1,2], [3,4], [5,7]] [(+3), id, (7-)]