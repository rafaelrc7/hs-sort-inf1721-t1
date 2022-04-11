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
main = executeInp >> putStrLn "Done."

executeInp :: IO [[[Int]]]
executeInp = do
  inp <- getContents
  let (nTests:ls) = lines inp
  let tests       = parseTests (read nTests) ls
  sequence $ executeTest <$> tests

parseTest :: [String] -> (String, [Int], [String])
parseTest (tName:tEls:ls) = (tName, ns, ls')
  where parseTest' :: Int -> [String] -> ([Int], [String])
        parseTest' 0 ls     = ([], ls)
        parseTest' n (l:ls) = (read l : ns, ls')
          where (ns, ls') = parseTest' (n-1) ls
        (ns, ls') = parseTest' (read tEls) ls

parseTests :: Int -> [String] -> [(String, [Int])]
parseTests 0 _  = []
parseTests n ls = (tLabel, tNums) : parseTests (n-1) ls'
  where (tLabel, tNums, ls') = parseTest ls

executeTest :: (String, [Int]) -> IO [[Int]]
executeTest (label, xs) = putStrLn ("\nExecuting test " ++ label)
                         >> benchmarkAll xs

benchmarkAll :: [Int] -> IO [[Int]]
benchmarkAll xs = do
  is  <- benchmark   I.sort   xs (Just "Insertion Sort")
  ms  <- benchmark   M.sort   xs (Just "Merge Sort")
  mqs <- benchmark   Q.msort  xs (Just "Mean Pivot Quick Sort")
  rqs <- benchmarkIO Q.rsort' xs (Just "Random Pivot Quick Sort")
  return [is, ms, mqs, rqs]

benchmark :: Ord a => ([a] -> [a]) -> [a] -> Maybe String -> IO [a]
benchmark sort xs msg = do
  start <- getTime Monotonic
  res   <- evaluate $ sort xs
  end   <- getTime Monotonic
  case msg of
    Just msg -> putStr (msg ++ " >> ")
                  >> fprint (timeSpecs % "\n") start end
    Nothing -> fprint (timeSpecs % "\n") start end
  return res

benchmarkIO :: Ord a => ([a] -> IO [a]) -> [a] -> Maybe String -> IO [a]
benchmarkIO sort xs msg = do
  start <- getTime Monotonic
  res   <- sort xs
  end   <- getTime Monotonic
  case msg of
    Just msg -> putStr (msg ++ " >> ")
                  >> fprint (timeSpecs % "\n") start end
    Nothing -> fprint (timeSpecs % "\n") start end
  return res

