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

executeTest :: Ord a => (String, [a]) -> IO [[a]]
executeTest (label, xs) = putStrLn ("\nExecuting test " ++ label)
                         >> benchmarkAll xs

benchmarkAll :: Ord a => [a] -> IO [[a]]
benchmarkAll xs = do
  is <- benchmark I.sort xs (Just "Insertion Sort")
  ms <- benchmark M.sort xs (Just "Merge Sort")
  qs <- benchmark Q.sort xs (Just "Quick Sort")
  return [is, ms, qs]

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

