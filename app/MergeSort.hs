module MergeSort (
  sort
) where

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge xs'@(x:xs) ys'@(y:ys)
  | x < y     = x : merge xs  ys'
  | otherwise = y : merge xs' ys

sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = merge (sort fstHalf) (sort sndHalf)
  where (fstHalf, sndHalf) = split xs

split :: [a] -> ([a], [a])
split xs = splitAt mid xs
  where mid = (length xs) `div` 2

