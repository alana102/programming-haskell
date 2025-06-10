-- Atividade 02 de Programação Funcional
-- Identificação
-- Alana Maria Sousa Augusto - 564976
-- Clara Cruz Alves - 568563

-- Questão 1
-- Fundir dois vetores ordenados num vetor ordenado maior.
-- use casamento de padrões.
-- não use meios externos de ordenação.
-- use recursão.
merge :: Ord a => [a] -> [a] -> [a]
merge [] v = v -- vetor esquerdo vazio (caso base)
merge u [] = u -- vetor direito vazio (caso base)
merge (u:us) (v:vs) -- caso geral
    | u <= v = u : merge us (v:vs)
    | otherwise = v : merge (u:us) vs

-- Questão 2
-- implemente mergesort para 
-- ordenação do vetor u.
-- Use a função anterior.
mergesort :: Ord a => [a] -> [a]
mergesort [] = [] -- vetor vazio
mergesort [u] = [u] -- vetor com só um elemento
mergesort us = merge (mergesort esq) (mergesort dir) -- caso geral
    where
        esq = take (div (length us) 2) us
        dir = drop (div (length us) 2) us
    

-- Questão 3
-- usando fold implementar função que retorne 
-- a série de Fibonacci com n elementos.
fibo'list :: Int -> [Int]
fibo'list n
    | n == 0 = [] -- 0 elementos
    | n == 1 = [1] -- 1 elemento
    | n == 2 = [1,1] -- 2 elementos
    | otherwise = reverse (foldl (\a _ -> (head a + head (tail a)) : a) [1,1] [3..n]) -- 3 ou mais elementos

