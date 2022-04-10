module Main where

import System.CPUTime
import qualified InsertionSort as I
import qualified MergeSort     as M
import qualified QuickSort     as Q

data BenchmarkOps = BenchmarkOps
  { checkResult :: Bool
  , printResult :: Bool
  }

main :: IO ()
main = putStrLn "Hello, Haskell!"

benchmark :: (Ord a, Fractional b) => [a] -> ([a] -> [a]) -> IO b
benchmark xs sort = do
  start <- getCPUTime
  let xs' = sort xs
  end <- getCPUTime
  return $ (fromIntegral (end - start)) / (10^12)

