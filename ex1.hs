import Prelude hiding (map, foldl, foldr, filter, flip, reverse, sum, replicate, id, zip, partition)
import Data.Function (on)
import Data.List (genericLength)

foldl :: (a -> e -> a) -> a -> [e] -> a
foldl op n l = fold l
    where fold [] = n
          fold (x:xs) = op (fold xs) x 

foldr :: (e -> a -> a) -> a -> [e] -> a
foldr op n l = fold n l
    where fold n [] = n
          fold n (x:xs) = fold (op x n) xs
          
cons :: a -> [a] -> [a]
cons e l = e:l

flip :: (a -> b -> c) -> (b -> a -> c)
flip f y x = f x y

map :: (a -> b) -> [a] -> [b]
map f l = foldl (\a e -> f e:a) [] l

reverse :: [a] -> [a]
reverse l = rev l []
    where rev [] l = l
          rev (x:xs) l = rev xs (x:l)

filter :: (a -> Bool) -> [a] -> [a]
filter p l = foldl (\a e -> if p e then e:a else a) [] l

factorial :: Integer -> Maybe Integer
factorial n = if n < 0 then Nothing else Just (foldr (*) 1 [1..n])

doubleFactorial :: Integer -> Maybe Integer
doubleFactorial n = if n < 0 then Nothing else Just (foldr (*) 1 $ filter (((==) `on` (`mod` 2)) n) [1..n])

fibonacci :: Integer -> Maybe Integer
fibonacci n = if n < 0 then Nothing else Just (seq 0 1 n)
    where seq a b n
                    | n == 0 = a
                    | n == 1 = b
                    | otherwise = seq b (a + b) (n - 1)

sum :: Integer -> Integer -> Integer
sum a b = foldl (+) 0 [a..b]

sumEven :: Integer -> Integer -> Integer
sumEven a b = foldl (+) 0 $ filter even [a..b]

revdigits :: Int -> [Int]
revdigits n
    | n < 10 = [n]
    | otherwise = (n `mod` 10):revdigits (n `div` 10)

reverseInt n = foldr (\d num -> 10 * num + d) 0 (revdigits n)

divisor :: Integer -> Integer -> Bool
divisor n x = (n `mod` x) == 0

sumDivisors :: Integer -> Integer
sumDivisors n = foldl (+) 0 $ filter (divisor n) [1..(abs n)]

palindrome :: Int -> Bool
palindrome n = n == reverseInt n

prime :: Integer -> Bool
prime n =  null $ filter (divisor n) [2..(n - 1)]

goldbach :: Integer -> Maybe (Integer, Integer)
goldbach n = if n < 3 then Nothing else g 2 (n - 2)
    where g a b
                | (prime a) && (prime b) = Just (a, b)
                | a > b = Nothing
                | otherwise = g (a + 1) (b - 1)

euler :: Integer -> Maybe Integer
euler n = if n < 1 then Nothing else Just (genericLength $ filter (not . (divisor n)) [2..(n - 1)]) 

constf :: b -> a -> b
constf c a = c

replacate :: Int -> a -> [a]
replacate n e = map (constf e) [1..n]

id :: a -> a
id x = x

applyMultiple :: Int -> (a -> a) -> a -> a
applyMultiple n f = foldr (.) id $ replacate n f

countp :: (Int -> Bool) -> Int -> Int -> Int
countp p a b = length $ filter p [a..b]

zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x, y):zip xs ys

compress :: (Eq a) => [a] -> [(a, Int)]
compress [] = []
compress [x] = [(x, 1)]
compress (x:xs) = if x == e then (x, c + 1):cs else (x, 1):l
    where l@((e, c):cs) = compress xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p l = foldl (\(pxs, npxs) x -> if p x then (x:pxs, npxs) else (pxs, x:npxs)) ([], []) l

count :: (Eq a) => [a] -> [(a, Int)]
count [] = []
count (x:xs) =
    let (eqx, neqx) = partition (x ==) xs
    in (x, length eqx + 1):count neqx

distance :: (Double, Double) -> (Double, Double) -> Double
distance (a1, b1) (a2, b2) = sqrt $ (a1 - a2)^2 + (b1 - b2)^2

maxDistance :: [(Double, Double)] -> Double
maxDistance ps = maximum $ map (\p -> maximum $ map (distance p) ps) ps

rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate [] _ = []
rotate xs i =
    let absi = abs i
        xsLength = length xs
        minRotate = absi `mod` xsLength
        splitLength = if i > 0 then minRotate else xsLength - minRotate
        (left, right) = split xs splitLength
    in right ++ left
    where split list count
                            | count == 0 = ([], list)
                            | otherwise =
                                let (left, (first:rest)) = split list (count - 1)
                                in (left ++ [first], rest)


extremal :: (a -> a -> Bool) -> [a] -> Maybe a
extrema _ [] = Nothing
extremal p (x:xs) = Just (foldl (\ext curr -> if p curr ext then curr else ext) x xs)


safeMinimum :: (Ord a) => [a] -> Maybe a
safeMinimum = extremal (<)

safeMaximum :: (Ord a) => [a] -> Maybe a
safeMaximum = extremal (>)

remdups :: (Eq a) => [a] -> [a]
remdups = foldl (\list curr -> if null list then [curr] else if curr == head list then list else curr:list) []

sufs :: [a] -> [[a]]
sufs = foldl (\list@(x:xs) curr -> ([curr] ++ x):list) [[]]

prefs :: [a] -> [[a]]
prefs = reverse . foldr (\curr list@(x:xs) -> (x ++ [curr]):list) [[]]

shift :: [a] -> [a]
shift = foldl (\l c -> if null l then [c] else (head l):c:(tail l)) []

rotates :: [a] -> [[a]]
rotates l = shift $ tail $ foldl (\rs@(h:_) _ -> (shift h):rs) [l] l

pairs :: Int -> [(Int, Int)]
pairs n = [(x, y) | x <- firstn, y <- firstn, x /= y]
    where firstn = [1..n]

compose :: (Eq b) => [(b, c)] -> [(a, b)] -> [(a, c)]
compose gl fl = [(x, z) | (x, y2) <- fl, (y1, z) <- gl, y1 == y2]

alternateCompose :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
alternateCompose f g 0 x = x
alternateCompose f g i x = f $ alternateCompose g f (i - 1) x

permutable :: Int -> Int -> (Int -> Int) -> (Int -> Int) -> Bool
permutable start end f g = all (\i -> alternateCompose f g i i == alternateCompose g f i i) [i | i <- [start..end], even i]


findMax :: (Int -> Int -> Int) -> Int -> Int -> Int
findMax f start end = maximum $ map (\(i, j) -> foldl (flip f) j [i..(j - 1)]) [(i, j) | i <- [start..(end - 1)], j <- [i..end]]

main :: IO ()
main = do
    print $ factorial 3
    print $ fibonacci 6
    print $ doubleFactorial 5
    print $ sum (-2) 3
    print $ sumEven (-3) 6
    print $ reverseInt 123
    print $ sumDivisors 5
    print $ palindrome 12321
    print $ prime 13
    print $ goldbach 28
    print $ euler 8
    print $ applyMultiple 3 sqrt 256
    print $ countp even 2 5
    print $ zip [2..4] [3..6]
    print $ compress [1, 1, 2, 3, 3, 3, 4, 2, 2, 2, 2, 1]
    print $ partition even [1..7]
    print $ count [1, 1, 2, 3, 3, 3, 4, 2, 2, 2, 2, 1]
    print $ maxDistance [(-1.1, 1), (1.8, 2), (3, 1), (-1, -2)]
    print $ rotate "abcdefgh" 3
    print $ rotate "abcdefgh" (-2)
    print $ safeMinimum [2, 3, 4, 1, 2, 5]
    print $ remdups [1, 2, 2, 3, 3, 3, 1, 1]
    print $ sufs "ivo"
    print $ prefs "ivo"
    print $ shift [1, 2, 3, 4]
    print $ rotates [1, 2, 3]
    print $ pairs 3
    print $ compose [(2, 3), (11, 14)] [(1, 2), (7, 11), (8, 10)]
    print $ alternateCompose (2 *) (2 +) 4 1
    print $ permutable 1 9 (\x -> x * x) (\x -> x * x * x)
    print $ findMax (-) 1 5