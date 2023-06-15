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

avaliaExpressao :: [(String, String, Char)] -> Node -> String -> Bool
avaliaExpressao grafo expressao estadoInicial
  | verificarTipoNo expressao == Folha =
    executaPrograma grafo estadoInicial (retornaProgramaFolha expressao) /= "False"
  | otherwise = False

lerArvoreExpressao :: Node -> String
lerArvoreExpressao (NoFolha c) = [c]
lerArvoreExpressao (NoUnario node c) = c : lerArvoreExpressao node
lerArvoreExpressao (NoBinario left right c) = c : lerArvoreExpressao left ++ lerArvoreExpressao right


main :: IO ()
main = do
    frame <- criaFrame
    traverseStructure frame
    let postfixExpression = "a"
    let raiz = criaArvoreExpressao postfixExpression
    putStrLn $ "Arvore de Expressao: " ++ show raiz
    putStrLn $ lerArvoreExpressao raiz
    print (avaliaExpressao frame raiz "w1")
