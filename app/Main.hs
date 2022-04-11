{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import Formatting        (fprint, (%))
import Formatting.Clock  (timeSpecs)
import System.Clock      (getTime, Clock(..))

import qualified InsertionSort as I
import qualified MergeSort     as M
import qualified QuickSort     as Q

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
executeTest (label, xs) =
  putStrLn ("\nExecuting test " ++ label)
    >> timeAll [(evaluate $ I.sort xs,  Just "Insertion Sort         "),
                (evaluate $ M.sort xs,  Just "Merge Sort             "),
                (evaluate $ Q.msort xs, Just "Mean Pivot Quick Sort  "),
                (Q.rsort' xs,           Just "Random Pivot QUick Sort")]

timeAll :: [(IO [a], Maybe String)] -> IO [[a]]
timeAll [] = return []
timeAll ((f, l):fs) = do r  <- timeIO  f  l
                         rs <- timeAll fs
                         return $ r:rs

timeIO :: IO a -> Maybe String -> IO a
timeIO f msg = do start <- getTime Monotonic
                  res   <- f
                  end   <- getTime Monotonic
                  case msg of
                    Just msg -> putStr (msg ++ " >> ")
                                  >> fprint (timeSpecs % "\n") start end
                    Nothing -> fprint (timeSpecs % "\n") start end
                  return res

