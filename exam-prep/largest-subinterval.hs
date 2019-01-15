import Data.Function

newtype Interval a = Interval {interval :: (a, a)} deriving (Eq)

intervalLength :: (Num a) => Interval a -> a
intervalLength (Interval (i, j)) = j - i 

instance (Ord a, Num a) => Ord (Interval a) where
    compare = compare `on` intervalLength

largestSubInterval :: (Integral z) => (z -> z) -> (z -> z) -> z -> z -> (z, z)
largestSubInterval f g a b =  let intervals = [(i, j) | i <- [a..b], j <- [i..b]]
                              in interval $ maximum $ map Interval $ filter isGood intervals
    where isGood (i, j) = all (\x -> (f x) == (g x)) [i..j]
          
main = print $ largestSubInterval (\x -> x) (\x -> x*x) 0 3