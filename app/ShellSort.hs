module ShellSort  where

import Control.Monad.ST
import Data.Array.ST
import Data.Array

sort :: Ord a => [a] -> (Int -> [Int]) -> [a]
sort []  _    = []
sort [x] _    = [x]
sort lst gaps = elems $ runSTArray $ do stArr <- thaw arr
                                        sort' stArr (gaps (max `div` 2)) max
                                        return stArr
                                          where arr      = listToArray lst
                                                (_, max) = bounds arr

sort' :: Ord a => STArray s Int a -> [Int] -> Int -> ST s ()
sort' arr []     _   = return ()
sort' arr (g:gs) max = (sortGap arr g max) >> sort' arr gs max

sortGap :: Ord a => STArray s Int a -> Int -> Int -> ST s ()
sortGap arr gap max = sortGap' gap max gap arr

sortGap' :: Ord a => Int -> Int -> Int -> STArray s Int a -> ST s ()
sortGap' offset max gap arr
  | offset <= max = sortGap' (offset+1) max gap arr >> sortGapStep arr offset gap
  | otherwise = return ()

sortGapStep :: Ord a => STArray s Int a -> Int -> Int -> ST s ()
sortGapStep arr offset gap = do val <- readArray arr offset
                                sortGapStep' arr val offset gap

sortGapStep' :: Ord a => STArray s Int a -> a -> Int -> Int -> ST s ()
sortGapStep' arr val offset gap
    | offset >= gap =
      do tmp <- readArray arr (offset-gap)
         if val < tmp then
           do readArray arr (offset-gap) >>= writeArray arr offset
              sortGapStep' arr val (offset-gap) gap
         else
           do writeArray arr offset val
    | otherwise =
           do writeArray arr offset val

listToArray :: [a] -> Array Int a
listToArray lst = listArray (0, (length lst) - 1) lst

shellGap :: Int -> [Int]
shellGap 0   = []
shellGap max = max : (shellGap $ max `div` 2)

fibGap :: Int -> [Int]
fibGap max = reverse $ takeWhile (< max) fibs

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

twoPowersGap :: Int -> [Int]
twoPowersGap max = reverse $ takeWhile (< max) twoPowers

twoPowers :: [Int]
twoPowers = iterate (*2)  1

