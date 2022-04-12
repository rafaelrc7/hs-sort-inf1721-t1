{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception  (evaluate)
import Control.Monad      (foldM)
import Data.List          (nub)
import Formatting         (fprint, (%))
import Formatting.Clock   (timeSpecs)
import System.Clock       (getTime, Clock(..))
import System.Environment (getArgs)

import qualified InsertionSort as I
import qualified MergeSort     as M
import qualified QuickSort     as Q
import qualified ShellSort     as S

data Flag = Validate
  deriving (Eq, Show)

main :: IO ()
main = do args <- getArgs
          case parseArgs args of
            Just ops -> executeInp (Validate `elem` ops) >> return ()
            Nothing  -> error "Invalid command line arguments."

getFlag :: String -> Maybe Flag
getFlag flag = lookup flag [("-V",         Validate),
                            ("--validate", Validate)]

parseArgs :: [String] -> Maybe [Flag]
parseArgs = foldM (\ops arg -> getFlag arg >>= \op -> return $ op:ops) []

executeInp :: Bool -> IO [[[Int]]]
executeInp validate = do
  inp <- getContents
  let (nTests:ls) = lines inp
  let tests       = parseTests (read nTests) ls
  results <- sequence $ (executeTest validate) <$> tests
  return results

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

executeTest :: Bool -> (String, [Int]) -> IO [[Int]]
executeTest validate (label, xs) =
  putStrLn ("\nExecuting test " ++ label)
    >> timeAll validate [(evaluate $ I.sort xs,                Just "Insertion Sort         "),
                         (evaluate $ M.sort xs,                Just "Merge Sort             "),
                         (evaluate $ S.sort xs S.shellGap,     Just "Half Shell sort        "),
                         (evaluate $ S.sort xs S.fibGap,       Just "Fibonacci Shell sort   "),
                         (evaluate $ S.sort xs S.twoPowersGap, Just "Two Power Shell sort   "),
                         (evaluate $ Q.msort xs,               Just "Mean Pivot Quick Sort  "),
                         (evaluate $ Q.msort xs,               Just "Mean Pivot Quick Sort  "),
                         (Q.rsort' xs,                         Just "Random Pivot Quick Sort")]

timeAll :: Ord a => Bool -> [(IO [a], Maybe String)] -> IO [[a]]
timeAll _        []          = return []
timeAll validate ((f, l):fs) = do r  <- timeIO  f  l
                                  if validate then
                                    if isSorted r then
                                      putStrLn "\tOK"
                                    else
                                      putStrLn "\tERROR"
                                  else
                                    putStrLn ""
                                  rs <- timeAll validate fs
                                  return $ r:rs

timeIO :: Ord a => IO [a] -> Maybe String -> IO [a]
timeIO f msg = do start <- getTime Monotonic
                  res   <- f
                  end   <- getTime Monotonic
                  case msg of
                    Just msg -> putStr (msg ++ " >> ")
                                  >> fprint (timeSpecs) start end
                    Nothing -> fprint (timeSpecs) start end
                  return res

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xys) | x > y     = False
                   | otherwise = isSorted $ y:xys

