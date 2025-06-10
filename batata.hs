data Batata = Pedra | Papel | Tesoura 

instance Eq Batata where
    (==) Pedra Pedra = True
    (==) Papel Papel = True
    (==) Tesoura Tesoura = True
    (==) _ _ = False

instance Ord Batata where
    compare Pedra Pedra = EQ -- igual
    compare Papel Papel = EQ -- igual
    compare Tesoura Tesoura = EQ -- igual
    compare Pedra Papel = LT -- menor
    compare Papel Tesoura = LT -- menor
    compare Tesoura Pedra = LT -- menor
    compare _ _ = GT -- casos em que é maior
    (>=) a b = (compare a b) == GT || (a==b)