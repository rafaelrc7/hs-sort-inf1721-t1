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

-- loop principal do shellsort que executa o insertion sort em cima de um
-- subarray composto por elementos separados por g posições
sort' :: Ord a => STArray s Int a -> [Int] -> Int -> ST s ()
-- Caso base: acabaram os gaps, portanto algorítimo terminou.
sort' arr []     _   = return ()
-- Passo recursivo (loop): faz o sort, usando a função sortGap, no subarray do gap g
sort' arr (g:gs) max = (sortGap arr g max) >> sort' arr gs max


-- loop do insertion sort sobre os subarrays formados por elementos separados
-- por um intervalo gap

-- Função que inicia o loop do insertion sort em cima dos subarrays formados
-- pelo gap
sortGap :: Ord a => STArray s Int a -> Int -> Int -> ST s ()
sortGap arr gap max = sortGap' gap max gap arr
  where sortGap' :: Ord a => Int -> Int -> Int -> STArray s Int a -> ST s ()
        -- Função recursiva do loop em si.
        sortGap' i max gap arr
          -- Chama a função que faz o sort em cima do subarray que começa no index i
          | i <= max  = sortGapStep arr i gap >> sortGap' (i+1) max gap arr
          -- Fim do loop
          | otherwise = return ()


-- Função que faz sort no subarray que começa em i e formado por elementos a
-- cada gap elementos

-- inicialização do loop do "insertion sort" em cima do h-array usando o dado gap
sortGapStep :: Ord a => STArray s Int a -> Int -> Int -> ST s ()
sortGapStep arr i gap = do temp <- readArray arr i     -- pega o número atual, que será comparado com anteriores
                           sortGapStep' arr temp i gap -- inicia o loop com a função recursiva
  where sortGapStep' :: Ord a => STArray s Int a -> a -> Int -> Int -> ST s ()
        sortGapStep' arr temp j gap
            | j >= gap =  -- executa se j for maior ou igual que o gap
              do arrjgap <- readArray arr (j-gap) -- lê o item da posição atual
                 if temp < arrjgap then -- compara o o item do início do loop
                   do writeArray arr j arrjgap
                      sortGapStep' arr temp (j-gap) gap -- move o item atual pra posição anterior caso seja maior que temp
                 else
                   writeArray arr j temp -- fim pois está ordenado
            | otherwise = writeArray arr j temp -- encerra o loop colocando o item movido na posição atual

-------------------------------------
-- FUNÇÕES AUXILIARES DE UTILIDADE --
-------------------------------------

-- Converte uma lista em um Array
listToArray :: [a] -> Array Int a
listToArray lst = listArray (0, (length lst) - 1) lst

---------------------------------------------------
-- FUNÇÕES PARA GERAR VALORES DE INTERVALO (GAP) --
---------------------------------------------------

-- Pega os elementos menores que max da lista de potências de dois
powersOfTwoGap :: Int -> [Int]
powersOfTwoGap max = reverse $ takeWhile (< max) powersOfTwo

-- Lista infinita de potências de dois
powersOfTwo :: [Int]
powersOfTwo = iterate (*2)  1

-- Gaps baseados na sequência 2^k - 1 (potencias de dois menos um).
hibbardGap :: Int -> [Int]
hibbardGap max = reverse $ takeWhile (<= max `div` 2) [2^k - 1 | k <- [0 ..]]

-- Recursivamente calcula o gap "padrão", começado pelo valor da metade do
-- tamanho da lista, que é dividido por dois até chegar em 1;
shellGap :: Int -> [Int]
shellGap 1   = []
shellGap max = let half = max `div` 2 in half : (shellGap $ half)

-- Pega os elementos menores que max da lista de Fibonacci
fibGap :: Int -> [Int]
fibGap max = reverse $ takeWhile (< max) fibs

-- Lista infinita dos números da sequência de Fibonacci
fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

