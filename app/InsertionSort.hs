module InsertionSort (
  sort
) where

-- Essa função insere um valor x em uma list
-- A lista é percorrida até achar um elemento maior que x, então x é inserido antes dele.
insert :: Ord a => [a] -> a -> [a]
insert [] x = [x] -- Caso base: em lista vazia o elemento é inserido direto
insert ys'@(y:ys) x
  | x < y = x:ys' -- Caso recursivo: caso x seja menor que a cabeça, y, da lista, insira aqui
  | otherwise = y : insert ys x -- Caso recursivo: caso o contrário, continua percorrendo a lista para achar a posição de x

-- Função do InsertionSort.
sort :: Ord a => [a] -> [a]
sort [] = []  -- Caso base: uma list vazia já está organizada
sort (x:xs) = insert (sort xs) x -- Caso recursivo: A função recursivamente navega pela dada lista, pegando a cabeça, x, e a inserindo e uma nova lista, usando a função insert acima, tendo no final uma lista organizada

