module ShellSort  where

import Control.Monad.ST
import Data.Array.ST
import Data.Array

sort :: Ord a => [a] -> (Int -> [Int]) -> [a]
sort []  _    = []
sort [x] _    = [x]
sort lst gaps = elems $ sort' arr (gaps (max `div` 2))
                  where arr      = listToArray lst
                        (_, max) = bounds arr

sort' :: Ord a => Array Int a -> [Int] -> Array Int a
sort' arr []     = arr
sort' arr (g:gs) = sort' (sortGap arr g) gs

sortGap :: Ord a => Array Int a -> Int -> Array Int a
sortGap arr gap = sortGap' gap max gap arr
                    where (_, max) = bounds arr

sortGap' :: Ord a => Int -> Int -> Int -> Array Int a -> Array Int a
sortGap' offset max gap arr
  | offset <= max = sortGap' (offset+1) max gap $ sortGapStep arr offset gap
  | otherwise = arr

sortGapStep :: Ord a => Array Int a -> Int -> Int -> Array Int a
sortGapStep arr offset gap = sortGapStep' arr (arr ! offset) offset gap

sortGapStep' :: Ord a => Array Int a -> a -> Int -> Int -> Array Int a
sortGapStep' arr val offset gap
  | offset >= gap && val < (arr ! (offset-gap)) =
      let arr' = runSTArray $ do stArr <- thaw arr
                                 readArray stArr (offset-gap) >>= writeArray stArr offset
                                 return stArr
      in sortGapStep' arr' val (offset-gap) gap
  | otherwise = runSTArray $ do stArr <- thaw arr
                                writeArray stArr offset val
                                return stArr

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

