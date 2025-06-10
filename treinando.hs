
doubleMe :: Num a => a -> a
doubleMe x = x + x
tripleMe :: Num a => a -> a
tripleMe x = x + x + x

-- define o tipo das variáveis da função e do retorno
-- variavel -> variavel -> retorno 
-- o retorno sempre vem por último
doubleUs :: Float -> Float -> Float 
doubleUs x y = doubleMe x + doubleMe y 

doubleSmallNumber :: Integer -> Integer
doubleSmallNumber x = if x > 100
                      then x
                      else x*2


gg :: [t1] -> (t1 -> t2 -> a) -> t2 -> [a]
gg ls ff n = [ff x n | x<-ls]

soma :: Integer -> Integer -> Integer
soma x y = x+y
-- Defino que a é um número e que todos as variaveis são 'a' e o
-- retorno também é 'a'
produto :: Num a => a-> a-> a
produto x y = x*y


reduce :: [t] -> (t -> t -> t) -> t
reduce [] _ = error "Lista vazia" -- tratamento de erro em caso de lista vazia
reduce ls f = 
    if length ls <= 1 then
        head ls
    else
        f (head ls) (reduce (tail ls) f)

fat :: (Num t, Enum t) => t -> t
fat n = reduce [n, n-1..1] produto
