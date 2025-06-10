data Batata = Pedra | Papel | Tesoura deriving (Show)

instance Eq Batata where
    (==) Pedra Pedra = True
    (==) Papel Papel = True
    (==) Tesoura Tesoura = True
    (==) _ _ = False

instance Ord Batata where
    (<) Pedra Papel = True
    (<) Papel Tesoura = True
    (<) Tesoura Pedra = True
    (<) _ _ = False
    (>) x y = not (x < y) && x /= y