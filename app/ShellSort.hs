module ShellSort  where

import Control.Monad.ST
import Data.Array.ST
import Data.Array

sort :: Ord a => [a] -> (Int -> [Int]) -> [a]
sort []  _    = []
sort [x] _    = [x]
sort lst gaps = elems $ runSTArray $ do stArr <- thaw arr
                                        sort' stArr (gaps max) max
                                        return stArr
                                          where arr      = listToArray lst
                                                (_, max) = bounds arr

sort' :: Ord a => STArray s Int a -> [Int] -> Int -> ST s ()
sort' arr []     _   = return ()
sort' arr (g:gs) max = (sortGap arr g max) >> sort' arr gs max

sortGap :: Ord a => STArray s Int a -> Int -> Int -> ST s ()
sortGap arr gap max = sortGap' gap max gap arr

sortGap' :: Ord a => Int -> Int -> Int -> STArray s Int a -> ST s ()
sortGap' i max gap arr
  | i <= max  = sortGapStep arr i gap >> sortGap' (i+1) max gap arr
  | otherwise = return ()

sortGapStep :: Ord a => STArray s Int a -> Int -> Int -> ST s ()
sortGapStep arr i gap = do temp <- readArray arr i
                           sortGapStep' arr temp i gap

sortGapStep' :: Ord a => STArray s Int a -> a -> Int -> Int -> ST s ()
sortGapStep' arr temp j gap
    | j >= gap =
      do arrjgap <- readArray arr (j-gap)
         if temp < arrjgap then
           do readArray arr (j-gap) >>= writeArray arr j
              sortGapStep' arr temp (j-gap) gap
         else
           do writeArray arr j temp
    | otherwise =
           do writeArray arr j temp

listToArray :: [a] -> Array Int a
listToArray lst = listArray (0, (length lst) - 1) lst

shellGap :: Int -> [Int]
shellGap 1   = []
shellGap max = let half = max `div` 2 in half : (shellGap $ half)

fibGap :: Int -> [Int]
fibGap max = reverse $ takeWhile (< max) fibs

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

twoPowersGap :: Int -> [Int]
twoPowersGap max = reverse $ takeWhile (< max) twoPowers

twoPowers :: [Int]
twoPowers = iterate (*2)  1

