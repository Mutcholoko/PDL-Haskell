module Main where

data MatrizAdjacencias = MatrizAdjacencias [[String]]

matrizVazia :: Int -> MatrizAdjacencias
matrizVazia tam = MatrizAdjacencias (replicate tam (replicate tam "testeAPAGUE"))

getPrograma :: MatrizAdjacencias -> Int -> Int -> String
getPrograma (MatrizAdjacencias matriz) linha coluna = (matriz !! linha) !! coluna

setPrograma :: MatrizAdjacencias -> Int -> Int -> String -> MatrizAdjacencias
setPrograma (MatrizAdjacencias matriz) linha coluna programa = 
    let (before, current:after) = splitAt linha matriz
        novaLinha = take coluna current ++ [programa] ++ drop (coluna + 1) current in MatrizAdjacencias (before ++ (novaLinha : after))

main :: IO ()
main = do 
    putStrLn "Comecando"
    let matriz = matrizVazia 2
    putStrLn (getPrograma matriz 1 1)
    