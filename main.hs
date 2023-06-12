import Data.Array

criaMatriz :: Int -> Int -> IO [[[String]]]
criaMatriz 0 _ = return []
criaMatriz numRows numCols = do
  row <- createRow numCols 1
  rest <- criaMatriz (numRows - 1) numCols
  return (row : rest)
  where
    createRow 0 _ = return []
    createRow n rowNum = do
      putStrLn ("Insira o elemento na posicao (" ++ show rowNum ++ ", " ++ show (numCols - n + 1) ++ "):")
      val <- getLine
      rest <- createRow (n - 1) rowNum
      return (words val : rest)

type Grafo = Array (Int, Int) [String]

grafo :: Int -> IO (Grafo)
grafo size = do
  matrixVals <- criaMatriz size size
  let indexedVals = [((i, j), (matrixVals !! (i-1)) !! (j-1)) | i <- [1..size], j <- [1..size]]
  return $ array ((1, 1), (size, size)) indexedVals

type W = (Int, [String])

estados :: Int -> IO [W]
estados size
    | size <= 0 = return []
    | otherwise = do
        putStrLn $ "Insira a string para o vertice " ++ show size ++ ":"
        strings <- getLine
        rest <- estados (size - 1)
        return $ (size, words strings) : rest

data Node = NoUnario Node Char | NoBinario Node Node Char | Folha Char deriving Show

criaArvoreExpressao :: [Char] -> Node
criaArvoreExpressao xs = criaArvore xs []
  where
    criaArvore [] stack = head stack
    criaArvore (x:xs) stack
      | isOperadorUnario x = do
          let child = head stack
          let new_node = NoUnario child x
          let updated_stack = tail stack
          criaArvore xs (new_node : updated_stack)
      | isOperador x = do
          let right = head stack
          let left = stack !! 1
          let new_node = NoBinario left right x
          let updated_stack = drop 2 stack
          criaArvore xs (new_node : updated_stack)
      | otherwise = criaArvore xs (Folha x : stack)

isOperadorUnario :: Char -> Bool
isOperadorUnario x = x `elem` "?~*"

isOperador :: Char -> Bool
isOperador x = x `elem` ";>^v"

main :: IO ()
main = do 
    putStrLn "Insira o numero de vertices:"
    vertices <- readLn
    matriz <- grafo vertices
    putStrLn $ "Matriz criada: " ++ show matriz
    set <- estados vertices
    print set
    let postfixExpression = "a?ab;?"
    let raiz = criaArvoreExpressao postfixExpression
    putStrLn $ "Arvore de Expressao: " ++ show raiz
