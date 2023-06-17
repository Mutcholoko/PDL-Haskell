import Data.List (any)
import Debug.Trace

debug = flip trace

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

retornaEstados :: [(String, String, Char)] -> [String]
retornaEstados vetor = map (\(_, estadoDestino, _) -> estadoDestino) vetor

validaPrograma :: [(String, String, Char)] -> String -> Char -> [String]
validaPrograma grafo estado programa
  | null [(origem, destino, prog) | (origem, destino , prog) <- grafo, origem == estado, prog == programa] = ["False"]
  | otherwise = retornaEstados [(origem, destino, prog) | (origem, destino , prog) <- grafo, origem == estado, prog == programa]

executaPrograma :: [(String, String, Char)] -> String -> Char -> [String]
executaPrograma grafoIncidente estadoAtual programa
  | hasValorDiferente "False" (validaPrograma grafoIncidente estadoAtual programa) = validaPrograma grafoIncidente estadoAtual programa
  | otherwise = ["False"]

executaOpIteracao :: [String] -> [(String, String, Char)] -> String -> Node -> [String]
executaOpIteracao res grafoIncidente estadoAtual noFolha
  | null res =
    res ++ [estadoAtual]
  | length res == 1 =
    res ++ executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noFolha)
  | length res > 1 =
    res ++ executaFuncOrdemAlta grafoIncidente (executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noFolha)) (retornaProgramaFolha noFolha)
  | otherwise = []

executaOpUnario :: [(String, String, Char)] -> Node -> String -> [String]
executaOpUnario grafoIncidente programa estadoAtual
  | recuperaOp programa == '?' = [estadoAtual]
  | recuperaOp programa == '*' =
    executaOpIteracao (executaOpIteracao (executaOpIteracao [] grafoIncidente estadoAtual (recuperaNoFolha programa)) grafoIncidente estadoAtual (recuperaNoFolha programa)) grafoIncidente estadoAtual (recuperaNoFolha programa)
  | otherwise = ["False"]

avaliaRamo :: [(String, String, Char)] -> String -> Node -> [String]
avaliaRamo grafo estado noRamo
  | estado /= "False" && hasValorDiferente "False" (executaOpBinario grafo noRamo estado) = executaOpBinario grafo noRamo estado
  | otherwise = ["False"]

executaFuncOrdemAlta :: [(String, String, Char)] -> [String] -> Char -> [String]
executaFuncOrdemAlta grafo listaEstados programa
  | all null ([executaPrograma grafo estado programa  | estado <- listaEstados]) = ["False"]
  | otherwise = concat [executaPrograma grafo estado programa | estado <- listaEstados]

executaFuncOrdemAltaEs :: [(String, String, Char)] -> [String] -> Node -> [String]
executaFuncOrdemAltaEs grafo listaEstados programa
  | all null ([avaliaEscolha grafo estado programa  | estado <- listaEstados]) = ["False"]
  | otherwise = concat [avaliaEscolha grafo estado programa | estado <- listaEstados]

executaFuncOrdemAltaBi :: [(String, String, Char)] -> [String] -> Node -> [String]
executaFuncOrdemAltaBi grafo listaEstados programa
  | all null ([executaOpBinario grafo programa estado  | estado <- listaEstados]) = ["False"]
  | otherwise = concat [executaOpBinario grafo programa estado | estado <- listaEstados]

executaOpSequencial :: [(String, String, Char)] -> String -> Node -> Node -> [String]
executaOpSequencial grafoIncidente estadoAtual noEsquerdo noDireito
  | verificarTipoNo noEsquerdo == Folha && verificarTipoNo noDireito == Folha =
    executaFuncOrdemAlta grafoIncidente (executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noEsquerdo)) (retornaProgramaFolha noDireito)
  | verificarTipoNo noEsquerdo == Binario && verificarTipoNo noDireito == Binario =
    executaFuncOrdemAltaBi grafoIncidente (executaOpBinario grafoIncidente noEsquerdo estadoAtual) noDireito
  | verificarTipoNo noEsquerdo == Binario && verificarTipoNo noDireito == Folha =
    executaFuncOrdemAlta grafoIncidente (avaliaRamo grafoIncidente estadoAtual noEsquerdo) (retornaProgramaFolha noDireito)
  | verificarTipoNo noEsquerdo == Folha && verificarTipoNo noDireito == Binario =
    executaFuncOrdemAlta grafoIncidente (avaliaRamo grafoIncidente estadoAtual noDireito) (retornaProgramaFolha noEsquerdo)
  | verificarTipoNo noEsquerdo == Unario && verificarTipoNo noDireito == Unario =
    executaFuncOrdemAltaEs grafoIncidente (avaliaEscolha grafoIncidente estadoAtual noEsquerdo) noDireito
  | verificarTipoNo noEsquerdo == Unario && verificarTipoNo noDireito == Folha =
    executaFuncOrdemAlta grafoIncidente (executaOpUnario grafoIncidente noEsquerdo estadoAtual) (retornaProgramaFolha noDireito)
  | verificarTipoNo noEsquerdo == Folha && verificarTipoNo noDireito == Unario =
    executaFuncOrdemAlta grafoIncidente (executaOpUnario grafoIncidente noDireito estadoAtual) (retornaProgramaFolha noEsquerdo)
  | verificarTipoNo noEsquerdo == Binario && verificarTipoNo noDireito == Unario =
    executaFuncOrdemAltaEs grafoIncidente (avaliaEscolha grafoIncidente estadoAtual noEsquerdo) noDireito
  | verificarTipoNo noEsquerdo == Unario && verificarTipoNo noDireito == Binario =
    executaFuncOrdemAltaEs grafoIncidente (avaliaEscolha grafoIncidente estadoAtual noDireito) noEsquerdo
  | otherwise = ["False"]

escolhaNaoDeterministica :: [(String, String, Char)] -> String -> Node -> [String]
escolhaNaoDeterministica grafoIncidente estadoAtual noRamo
  | hasValorDiferente "False" (executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noRamo)) =
    executaPrograma grafoIncidente estadoAtual (retornaProgramaFolha noRamo)
  | otherwise = ["False"]

avaliaEscolha :: [(String, String, Char)] -> String -> Node -> [String]
avaliaEscolha grafo estado noRamo
  | verificarTipoNo noRamo == Binario =
    executaOpBinario grafo noRamo estado
  | verificarTipoNo noRamo == Unario =
    executaOpUnario grafo noRamo estado
  | verificarTipoNo noRamo == Folha =
    escolhaNaoDeterministica grafo estado noRamo

retornaEscolha :: [String] -> [String] -> [String]
retornaEscolha op1 op2
  | hasValorDiferente "False" op1 = op1
  | hasValorDiferente "False" op2 = op2
  | otherwise = ["False"]

executaOpEscolha :: [(String, String, Char)] -> String -> Node -> Node -> [String]
executaOpEscolha grafoIncidente estadoAtual noEsquerdo noDireito
  | hasValorDiferente "False" (retornaEscolha (avaliaEscolha grafoIncidente estadoAtual noEsquerdo) (avaliaEscolha grafoIncidente estadoAtual noDireito)) = retornaEscolha (avaliaEscolha grafoIncidente estadoAtual noEsquerdo) (avaliaEscolha grafoIncidente estadoAtual noDireito)
  | otherwise = ["False"]

executaOpBinario :: [(String, String, Char)] -> Node -> String -> [String]
executaOpBinario grafoIncidente programa estadoAtual
  | recuperaOp programa == ';' =
    executaOpSequencial grafoIncidente estadoAtual (recuperaNoEsquerdo programa) (recuperaNoDireito programa)
  | recuperaOp programa == 'U' =
    executaOpEscolha grafoIncidente estadoAtual (recuperaNoEsquerdo programa) (recuperaNoDireito programa)
  | otherwise = ["False"]

avaliaExpressao :: [(String, String, Char)] -> Node -> String -> Bool
avaliaExpressao grafo expressao estadoInicial
  | verificarTipoNo expressao == Folha =
    hasValorDiferente "False" (executaPrograma grafo estadoInicial (retornaProgramaFolha expressao))
  | verificarTipoNo expressao == Unario =
    hasValorDiferente "False" (executaOpUnario grafo expressao estadoInicial)
  | verificarTipoNo expressao == Binario =
    hasValorDiferente "False" (executaOpBinario grafo expressao estadoInicial)
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
    let postfixExpression = "a*b;"
    let raiz = criaArvoreExpressao postfixExpression
    putStrLn $ "Arvore de Expressao: " ++ show raiz
    putStrLn $ lerArvoreExpressao raiz
    print (avaliaExpressao frame raiz "w1")
