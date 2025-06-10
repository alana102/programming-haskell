-- Tipo de dado formato
data Shape = Circle Point Float | Rectangle Float Float deriving (Show)
data Point = Point Float Float deriving (Show)

-- Calcula a área
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle x1 y2) = x1 * y2

-- Muda o circulo de coordenada
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a)(y+b)) r

-- 1 nome, 2 nome, idade, altura, numero de tel, sorvete fav
data Person = Person String String Int Float String String deriving (Show)


firstName :: Person -> String
firstName (Person f _ _ _ _ _) = f

lastName :: Person -> String
lastName (Person _ l _ _ _ _) = l -- ... flavor

 -- Já cria as funções separadamente
data Cat = Cat { name :: String,
                 age :: Int,
                 breed :: String,
                 weight :: Float
                } deriving (Show) 

data Car a b c = Car { company :: a
                ,  model :: b
                ,  year :: c
                } deriving (Show)

data List a = None | Node a (List a) deriving (Show)

inserir :: a -> List a -> List a
inserir = Node

printls :: Show a => List a -> [Char]
printls None = ""
printls (Node key ls) = show key ++ "," ++ printls ls

printls' :: Show a => List a -> [Char]
printls' ls = "[" ++ init (printls ls) ++ "]"

mdc a b = if b == 0 then  a else mdc b (mod a b)  

data Frac = Frac Int Int

instance Show Frac where
    show (Frac n d) = "(" ++ show (n `div` m) ++ "/" ++ show (d `div` m) ++ ")"
        where m = mdc n d

instance Num Frac where
    (+) (Frac a m) (Frac b n) = Frac (a*n+b*m) (m*n)
    (-) (Frac a m) (Frac b n) = Frac (a*n - b*m) (m*n)
    (*) (Frac a m) (Frac b n) = Frac (a*b) (m*n)
    abs (Frac a m) = Frac (abs a) (abs m)
    signum (Frac a m) = Frac (signum (a * m)) 1
    fromInteger n = Frac (fromInteger n) 1