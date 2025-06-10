-- Atividade 01 de Programação Funcional
-- Identificação
ident :: [(String, String)]
ident = [("Alana", "564976"), ("Clara", "568563")]

-- 1ª questão: Função que receba um numero natural positivo,
-- e gere uma lista de tuplas onde cada tupla é formada
-- por um fator primo e segundo a potência desse número
eprimo :: Integral a => a -> Bool
eprimo x = length [ n | n <- [2, 3..x-1], mod x n == 0] == 0
listaPrimos :: Integral a => a -> [a]
listaPrimos x = [n | n<-[2..x], eprimo n, mod x n == 0]
contaOcorrencias :: Eq a => a -> [a] -> Int
contaOcorrencias n ls = length [x | x <- ls, x==n]

fatorar :: Int -> [Int]
fatorar 1 = []
fatorar n = if eprimo n then [n] else f: fatorar (n `div` f)  where f = menorFator n

menorFator :: Int -> Int
menorFator n = head [x | x <- [2 .. floor (sqrt (fromIntegral n))], n `mod` x == 0]

tprimos :: Int -> [(Int, Int)]
tprimos a = [(p, contaOcorrencias p fatores) | p <- listaPrimos a]
  where fatores = fatorar a

-- 2ª questão: seja uma string s da qual se deseja construir a lista das frequências
-- dos caracteres. Cada frequência é uma tupla formada pelo caractere e o total de 
-- vezes que ele aparece

freq :: String -> [(Char, Int)]
freq s = [(x, y) | x<-['0'..'z'], let y = contaOcorrencias x s, y /= 0]





