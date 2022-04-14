module QuickSort (
    sort
  , rsort
  , rsort'
  , msort
) where

import System.Random

-- Faz quick sort usando sempre o primeiro elemento da lista como pivot
sort :: Ord a => [a] -> [a]
sort []         = []
sort [a]        = [a]
sort xs'@(x:xs) = (sort less) ++ equal ++ (sort greater)
  where (less, equal, greater) = part x xs'

-- Faz o quick sort usando um valor aleatório como pivot
-- Para manter a pureza da função, além de retornar a lista organizada, a função também retorna o estado do gerador de números aleatórios.
rsort :: Ord a => [a] -> StdGen -> ([a], StdGen)
rsort []  g = ([],  g) -- Caso base: lista vazia já está organizada
rsort [a] g = ([a], g) -- Caso base: lista com um elemento já está organizada
rsort xs  g = (left ++ equal ++ right, g''') -- Caso recursivo: juntar as listas de elementos menores, iguais e maiores que o elemento no index aleatório
  where (i, g') = randomR (0, (length xs) - 1) g -- gera um número aleatório no intervalo [0, n) sendo n o tamanho da lista. Retorna também um novo estado do gerador de números aleatórios que é passado para frente
        (less, equal, greater) = part (xs !! i) xs -- particiona a lista de acordo com o valor no index aleatório
        (left, g'')            = rsort less    g'  -- recursivamente organiza a lista de valores menores que o pivot, salvando o novo estado do gerador de números aleatórios
        (right, g''')          = rsort greater g'' -- recursivamente organiza a lista de valores maiores que o pivot

-- Função auxiliar, não pura, que gera um estado novo para o gerador de números aleatórios.
rsort' :: Ord a => [a] -> IO [a]
rsort' xs = fst <$> (getStdGen >>= return . rsort xs)

-- Faz o quick sort usando o valor médio como pivot
msort :: [Int] -> [Int]
msort []         = []  -- Caso base: lista vazia já está organizada
msort [a]        = [a] -- Caso base: lista com um elemento já está organizada
msort xs = (msort less) ++ equal ++ (msort greater) -- Caso recursivo: quebra a lista em três listas: valores menores, iguais e maiores que o pivot, organizando as sublistas de menores e maiores recursivamente e então juntando tudo
  where mean :: [Int] -> Int  -- Funç~ao auxiliar que calcula o valor médio dos elementos de uma lista
        mean xs = sum xs `div` (length xs)
        (less, equal, greater) = part (mean xs) xs

-- Dado um elemento p e uma lista xs, xs é quebrado em três listas, respectivamente: a lista com elementos menores que p, a lista com elementos iguais a p e elementos maiores que p.
part :: Ord a => a -> [a] -> ([a], [a], [a])
part p xs = part' p xs ([], [], []) -- Inicia recursão com função auxiliar recursiva part', passando o estado inicial: três listas vazias
  where part' :: Ord a => a -> [a] -> ([a], [a], [a]) -> ([a], [a], [a])
        part' _ [] ls = ls        -- Caso base, xs está vazio, portanto processo finalizado
        part' p (x:xs) (l, e, g)  -- Caso recursivo, compara x com p e insere na lista apropriada
          | x < p     = part' p xs (x:l, e, g) -- x é menor que p, inserido na primeira lista
          | x > p     = part' p xs (l, e, x:g) -- x é maior que p, inserido na terceira lista
          | otherwise = part' p xs (l, x:e, g) -- x é igual a p, inserido na segunda lista

