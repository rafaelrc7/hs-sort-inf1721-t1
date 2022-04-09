module InsertionSort (
  sort
) where

insert :: Ord a => [a] -> a -> [a]
insert [] x = [x]
insert ys'@(y:ys) x
  | x < y = x:ys'
  | otherwise = y : insert ys x

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert (sort xs) x

