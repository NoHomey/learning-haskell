module Main where

import Data.List

divs :: Integer -> [Integer]
divs n = filter ((== 0) . (mod n)) [1..n]

stream :: Integer -> Integer -> [Integer]
stream k l = filter p [1..]
    where num (x, y) = x^k * y^l
          p n = let ds = divs n
                in any ((== n) . num) [(x, y) | x <- ds, y <- ds] 

generatePowers :: Integer -> Integer -> [Integer]
generatePowers k l = filter ((== 1) . (d l) . (d k)) [1..]
    where d x n = if n `mod` x == 0
                    then d x $ n `div` x
                    else n

golomb :: [Integer]
golomb = 1:next [1]
    where next seq = let g = gen seq 1 1
                     in g ++ next (seq ++ g)
            where gen []    0 e = [e + 1]
                  gen []    c e = genericReplicate c e
                  gen l     0 e = gen l (seq `genericIndex` e) $ e + 1
                  gen (_:t) c e = gen t (c - 1) e

kolakoski :: [Int]
kolakoski = gen [1] 1
    where other 1 = 2
          other 2 = 1
          gen (h:t) e = (rep h e) ++ (gen (t ++ [other e]) $ other h)
          rep 1 x = [x]
          rep 2 x = [x, x]

main = do
         print $ take 10 $ stream 2 3
         print $ take 10 $ generatePowers 2 3
         print $ take 5 golomb
         print $ take 11 golomb
         print $ take 30 golomb
         print $ take 40 golomb
         print $ take 7 kolakoski