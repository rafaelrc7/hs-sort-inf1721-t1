module QuickSort (
    sort
  , rsort
  , rsort'
  , msort
) where

import System.Random

sort :: Ord a => [a] -> [a]
sort []         = []
sort [a]        = [a]
sort xs'@(x:xs) = (sort less) ++ equal ++ (sort greater)
  where (less, equal, greater) = part x xs'

rsort :: Ord a => [a] -> StdGen -> ([a], StdGen)
rsort []  g = ([],  g)
rsort [a] g = ([a], g)
rsort xs  g = (left ++ equal ++ right, g''')
  where (i, g') = randomR (0, (length xs) - 1) g
        (less, equal, greater) = part (xs !! i) xs
        (left, g'')            = rsort less    g'
        (right, g''')          = rsort greater g''

rsort' :: Ord a => [a] -> IO [a]
rsort' xs = fst <$> (getStdGen >>= return . rsort xs)

msort :: [Int] -> [Int]
msort []         = []
msort [a]        = [a]
msort xs = (msort less) ++ equal ++ (msort greater)
  where mean :: [Int] -> Int
        mean xs = sum xs `div` (length xs)
        (less, equal, greater) = part (mean xs) xs

part :: Ord a => a -> [a] -> ([a], [a], [a])
part p xs = part' p xs ([], [], [])
  where part' :: Ord a => a -> [a] -> ([a], [a], [a]) -> ([a], [a], [a])
        part' _ [] ls = ls
        part' p (x:xs) (l, e, g)
          | x < p     = part' p xs (x:l, e, g)
          | x > p     = part' p xs (l, e, x:g)
          | otherwise = part' p xs (l, x:e, g)

