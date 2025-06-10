-- Questão 1
soma :: Num a => a -> a -> a
soma x y = x+y

-- Questão 2
menor x y
  | x < y = x
  | y < x = y
  | otherwise = error "Variáveis iguais"

-- Questão 3
menor3 :: Integer -> Integer -> Integer -> Integer
menor3 x y z
 | x < y && x < z = x
 | y < x && y < z = y
 | z < x && z < y = z
 | otherwise = error "Variáveis iguais"

-- Questão 4
areaCircunf :: Floating a => a -> a
areaCircunf x = pi * (x*x)

-- Questão 5
andTres :: Bool -> Bool -> Bool -> Bool
andTres b1 b2 b3 = b1 && b2 && b3

-- Questão 7
eprimo :: Integral a => a -> Bool
eprimo x = length [ n | n <- [2, 3..x-1], mod x n == 0] == 0

-- Questão 8
fatorial 0 = 1 -- caso base
fatorial x = x * fatorial (x-1) -- caso geral

-- Questão 9
fib 1 = 1 -- caso base
fib 2 = 1 -- caso base
fib n = fib (n-1) + fib (n-2) -- caso geral

-- Questão 10
elemento ls n = ls !! n-1

-- Questão 11
pertence ls n = n `elem` ls

-- Questão 12
nroElementos ls = sum [1 | x <-ls]

-- 13

-- 14
contaOcorrencias :: Eq a => a -> [a] -> Int
contaOcorrencias n ls = length [x | x <- ls, x==n]

-- 15
unicaOcorrencia n ls = contaOcorrencias ls n == 1

-- 16
maioresQue :: Ord a => a -> [a] -> [a]
maioresQue n ls = [x | x <- ls, x>n]

-- concatena
concatena l1 l2 = l1 ++ l2

-- duplica
duplica ls = [ y | x <- ls, y<-[x, x]]

-- passagem e idade
passagem p i
 | i >= 60 = p*(60/100)
 | i <= 10 && i >= 2 = p*(50/100)
 | i < 2 = p*(10/100)
 | otherwise = p

-- celsius para farenheit
conversao c = c*1.8+32


--é par
epar n = mod n 2 == 0

--media da lista
media ls = sum ls / fromIntegral (length ls)

-- tamanho da lista com elementos maiores que n na ls
retornaSup n ls = length [x | x <-ls, x>n]

-- Palindromo (escreve entre aspas as palavras)
verificaPalindromo ls = ls == reverse ls

-- intercala
intercala l1 l2 = [w | x <- l1, y <- l2, w <- [x, y]]


-- transforma uma letra em maiúsculo
mais' ch
    | ch `elem` x = head ls
    | otherwise = ch

  where
    ls=[snd t|t<-xy,fst t==ch] -- fst (first) snd (second)
    x=['a'..'z']
    y=['A'..'Z']
    xy=zip x y

capWord w = mais' h :  t -- concatena uma lista com seu começo 
  where h = head w -- primeiro elemento
        t = tail w -- resto da lista

capAllWord w = [mais' x | x<-w]


soma1 ls = [ x+1 | x<-ls ]


listaPrimos x = [n | n<-[2..x], eprimo n, mod x n == 0]


fatorar 1 = []
fatorar n = if eprimo n then [n] else menorFator n : fatorar (n `div` menorFator n)
menorFator n = head[x| x <- [2..sqrt n], n `mod` x == 0]


