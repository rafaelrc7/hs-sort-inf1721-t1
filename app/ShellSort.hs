module ShellSort  where

import Control.Monad.ST
import Data.Array.ST
import Data.Array

-- Função do shell sort, recebe uma lista e também uma função geradora de gap
sort :: Ord a => [a] -> (Int -> [Int]) -> [a]
sort []  _    = []  -- Caso base: lista vazia já está organizada
sort [x] _    = [x] -- Caso base: lista com um elemento já está organizada
sort lst gaps = elems $ runSTArray $ do stArr <- thaw arr -- transforma o array em um array mutável
                                        sort' stArr (gaps max) max -- chama a função auxiliar recursiva que inicia o sort, gerando a lista de gaps a partir do tamanho do array
                                        return stArr -- faz do array imutável de novo, que é então transformado novamente em uma lista pela função "elems"
                                          where arr      = listToArray lst -- transforma lista lst em array imutavel arr
                                                (_, max) = bounds arr -- pega o indice máximo do array

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

-- Converte uma lista em um Array
listToArray :: [a] -> Array Int a
listToArray lst = listArray (0, (length lst) - 1) lst

-- Recursivamente calcula o gap "padrão", começado pelo valor da metade do tamanho da lista, que é dividido por dois até chegar em 1;
shellGap :: Int -> [Int]
shellGap 1   = []
shellGap max = let half = max `div` 2 in half : (shellGap $ half)

-- Pega os elementos menores que max da lista de Fibonacci
fibGap :: Int -> [Int]
fibGap max = reverse $ takeWhile (< max) fibs

-- Lista infinita dos números da sequência de Fibonacci
fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- Pega os elementos menores que max da lista de potências de dois
twoPowersGap :: Int -> [Int]
twoPowersGap max = reverse $ takeWhile (< max) twoPowers

-- Lista infinita de potências de dois
twoPowers :: [Int]
twoPowers = iterate (*2)  1

