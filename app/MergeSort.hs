module MergeSort (
  sort
) where


-- Função do Merge sort
sort :: Ord a => [a] -> [a]
sort [] = [] -- Caso base: lista vazia já está organizada
sort [x] = [x] -- Caso base: lista com um elemento já está organizada
sort xs = merge (sort fstHalf) (sort sndHalf) -- Caso recursivo: junta as duas metades organizadas da lista xs
  where (fstHalf, sndHalf) = split xs         -- Essa linha quebra a lista xs na metada

-- Junta duas listas, assumidamente organizadas, mantendo os itens da lista final organizados
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs -- Caso base: juntando uma lista xs com uma lista vazia temos xs
merge [] xs = xs -- Caso base: juntando uma lista vazia com xs temos xs
merge xs'@(x:xs) ys'@(y:ys) -- Caso recursivo: pegamos as cabeças de ambas as listas, x e y, e inserimos o menor elemento dos dois primeiro, mantendo a outra lista como está para o próximo passo.
  | x < y     = x : merge xs  ys' -- x é menor, inserimos x na lista e passamos a segunda lista inteira para frente
  | otherwise = y : merge xs' ys -- x não é maior, inserimos y na lista e passamos a primeira lista inteira para frente

-- Separa uma lista em duas, pela metade
split :: [a] -> ([a], [a])
split xs = splitAt mid xs
  where mid = (length xs) `div` 2

