import Data.List (any)

criaFrame :: IO [(String, String, Char)]
criaFrame = do
  putStrLn "Digite uma relação do grafo (ou 'pronto' para sair):"
  relacao <- getLine
  if relacao == "pronto" then
    return []
  else do
    let frame = (words relacao !! 0, words relacao !! 1, head (words relacao !! 2))
    frames <- criaFrame
    return (frame : frames)

traverseStructure :: [(String, String, Char)] -> IO ()
traverseStructure [] = putStrLn "Acabou."
traverseStructure ((str1, str2, ch) : rest) = do
  putStrLn $ "Estado origem: " ++ str1
  putStrLn $ "Estado destino: " ++ str2
  putStrLn $ "Programa: " ++ [ch]
  putStrLn "------"
  traverseStructure rest

data Node = NoUnario Node Char | NoBinario Node Node Char | NoFolha Char deriving Show

data NodeType = Binario | Unario | Folha deriving (Show, Eq)

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
      | isOperadorBinario x = do
          let right = head stack
          let left = stack !! 1
          let new_node = NoBinario left right x
          let updated_stack = drop 2 stack
          criaArvore xs (new_node : updated_stack)
      | otherwise = criaArvore xs (NoFolha x : stack)

isOperadorUnario :: Char -> Bool
isOperadorUnario x = x `elem` "?*"

isOperadorBinario :: Char -> Bool
isOperadorBinario x = x `elem` ";U"

verificarTipoNo :: Node -> NodeType
verificarTipoNo (NoFolha _) = Folha
verificarTipoNo (NoUnario _ _) = Unario
verificarTipoNo (NoBinario _ _ _) = Binario

retornaProgramaFolha :: Node -> Char
retornaProgramaFolha (NoFolha c) = c

retornaEstado :: (String, String, Char) -> String
retornaEstado ( _ ,estadoDestino, _ ) = estadoDestino

validaPrograma :: [(String, String, Char)] -> String -> Char -> String
validaPrograma grafo estado programa
  | null [(origem, destino, prog) | (origem, destino , prog) <- grafo, origem == estado, prog == programa] = "NULL"
  | otherwise = retornaEstado $ head [(origem, destino, prog) | (origem, destino , prog) <- grafo, origem == estado, prog == programa]

executaPrograma :: [(String, String, Char)] -> String -> Char -> String
executaPrograma grafoIncidente estadoAtual programa
  | validaPrograma grafoIncidente estadoAtual programa == "NULL" = "False"
  | otherwise = validaPrograma grafoIncidente estadoAtual programa

executaOpIteracao :: [String] -> [(String, String, Char)] -> String -> Node -> [String]
executaOpIteracao res grafoIncidente estadoAtual noFolha
  | null res =
    res ++ [estadoAtual]
  | length res == 1 =
    res ++ [executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noFolha)]
  | length res == 2 =
    res ++ [executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noFolha)]
  | otherwise = []

executaOpUnario :: [(String, String, Char)] -> Node -> String -> [String]
executaOpUnario grafoIncidente programa estadoAtual
  | recuperaOp programa == '?' = [estadoAtual]
  | recuperaOp programa == '*' =
    executaOpIteracao (executaOpIteracao (executaOpIteracao [] grafoIncidente estadoAtual (recuperaNoFolha programa)) grafoIncidente estadoAtual (recuperaNoFolha programa)) grafoIncidente estadoAtual (recuperaNoFolha programa)
  | otherwise = ["False"]

executaOpSequencial :: [(String, String, Char)] -> String -> Node -> Node -> String
executaOpSequencial grafoIncidente estadoAtual noEsquerdo noDireito
  | verificarTipoNo noEsquerdo == Folha && verificarTipoNo noDireito == Folha =
    executaPrograma grafoIncidente (executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noEsquerdo)) (retornaProgramaFolha noDireito)
  | otherwise = "False"

escolhaNaoDeterministica :: [(String, String, Char)] -> String -> Node -> Node -> String
escolhaNaoDeterministica grafoIncidente estadoAtual noEsquerdo noDireito
  | executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noEsquerdo) /= "False" =
    executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noEsquerdo)
  | executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noDireito) /= "False" =
    executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noDireito)
  | otherwise = "False"

executaOpEscolha :: [(String, String, Char)] -> String -> Node -> Node -> String
executaOpEscolha grafoIncidente estadoAtual noEsquerdo noDireito
  | verificarTipoNo noEsquerdo == Folha && verificarTipoNo noDireito == Folha =
    escolhaNaoDeterministica grafoIncidente estadoAtual noEsquerdo noDireito
  | otherwise = "False"

executaOpBinario :: [(String, String, Char)] -> Node -> String -> String
executaOpBinario grafoIncidente programa estadoAtual
  | recuperaOp programa == ';' =
    executaOpSequencial grafoIncidente estadoAtual (recuperaNoEsquerdo programa) (recuperaNoDireito programa)
  | recuperaOp programa == 'U' =
    executaOpEscolha grafoIncidente estadoAtual (recuperaNoEsquerdo programa) (recuperaNoDireito programa)
  | otherwise = "False"

avaliaExpressao :: [(String, String, Char)] -> Node -> String -> Bool
avaliaExpressao grafo expressao estadoInicial
  | verificarTipoNo expressao == Folha =
    executaPrograma grafo estadoInicial (retornaProgramaFolha expressao) /= "False"
  | verificarTipoNo expressao == Unario =
    hasValorDiferente "False" (executaOpUnario grafo expressao estadoInicial)
  | verificarTipoNo expressao == Binario =
    executaOpBinario grafo expressao estadoInicial /= "False"
  | otherwise = False

recuperaOp :: Node -> Char
recuperaOp (NoUnario node c) = c
recuperaOp (NoBinario left right c) = c

recuperaNoFolha :: Node -> Node
recuperaNoFolha (NoUnario node c) = node

recuperaNoEsquerdo :: Node -> Node
recuperaNoEsquerdo (NoBinario left right c) = left

recuperaNoDireito :: Node -> Node
recuperaNoDireito (NoBinario left right c) = right

hasValorDiferente :: String -> [String] -> Bool
hasValorDiferente x vetor = any (/= x) vetor

lerArvoreExpressao :: Node -> String
lerArvoreExpressao (NoFolha c) = [c]
lerArvoreExpressao (NoUnario node c) = c : lerArvoreExpressao node
lerArvoreExpressao (NoBinario left right c) = c : lerArvoreExpressao left ++ lerArvoreExpressao right

main :: IO ()
main = do
    frame <- criaFrame
    traverseStructure frame
    let postfixExpression = "a*"
    let raiz = criaArvoreExpressao postfixExpression
    putStrLn $ "Arvore de Expressao: " ++ show raiz
    putStrLn $ lerArvoreExpressao raiz
    print (avaliaExpressao frame raiz "w1")
