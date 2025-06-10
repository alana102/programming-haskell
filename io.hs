imc p a = p / (a**2)
main = do 
    putStrLn "digite seu nome"
    nome <- getLine
    putStrLn "digite sua idade"
    idade <- getLine
    putStrLn "digite seu peso"
    peso <- getLine
    putStrLn "digite sua altura"
    altura <- getLine
    let pesoNum = read peso :: Float -- transforma em float
    let alturaNum = read altura :: Float -- transforma em float
    putStrLn ("Idade de " ++ nome ++ ": " ++ idade)
    putStrLn ("Seu imc é " ++ show (imc pesoNum alturaNum))