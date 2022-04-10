{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import Formatting        (fprint, (%))
import Formatting.Clock  (timeSpecs)
import System.Clock      (getTime, Clock(..))

import qualified InsertionSort as I
import qualified MergeSort     as M
import qualified QuickSort     as Q

data BenchmarkOps = BenchmarkOps
  { checkResult :: Bool
  , printResult :: Bool
  }

main :: IO ()
main = putStrLn "Hello, Haskell!"

benchmarkAll :: Ord a => [a] -> IO [[a]]
benchmarkAll xs = do
  is <- benchmark I.sort xs (Just "Insertion Sort")
  ms <- benchmark M.sort xs (Just "Merge Sort")
  qs <- benchmark Q.sort xs (Just "Quick Sort")
  return [is, ms, qs]

benchmark :: Ord a => ([a] -> [a]) -> [a] -> Maybe String -> IO [a]
benchmark sort xs msg = do
  start <- getTime Monotonic
  let res = evaluate $ sort xs
  end <- getTime Monotonic
  case msg of Just msg -> putStr (msg ++ " >> ")
                >> fprint (timeSpecs % "\n") start end
              Nothing -> fprint (timeSpecs % "\n") start end
  res

