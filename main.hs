import Data.Array

createMatrix :: Int -> Int -> IO [[[String]]]
createMatrix 0 _ = return []
createMatrix numRows numCols = do
  row <- createRow numCols 1
  rest <- createMatrix (numRows - 1) numCols
  return (row : rest)
  where
    createRow 0 _ = return []
    createRow n rowNum = do
      putStrLn ("Enter element at position (" ++ show rowNum ++ ", " ++ show (numCols - n + 1) ++ "):")
      val <- getLine
      rest <- createRow (n - 1) rowNum
      return (words val : rest)

type Graph = Array (Int, Int) [String]

graph :: Int -> IO (Graph)
graph size = do
  matrixVals <- createMatrix size size
  let indexedVals = [((i, j), (matrixVals !! (i-1)) !! (j-1)) | i <- [1..size], j <- [1..size]]
  return $ array ((1, 1), (size, size)) indexedVals

type W = (Int, [String])

states :: Int -> IO [W]
states size
    | size <= 0 = return []
    | otherwise = do
        putStrLn $ "Enter strings for vertex " ++ show size ++ ":"
        strings <- getLine
        rest <- states (size - 1)
        return $ (size, words strings) : rest

data Node = UnaryNode Node Char | BinaryNode Node Node Char | Leaf Char deriving Show

buildExpressionTree :: [Char] -> Node
buildExpressionTree xs = buildTree xs []
  where
    buildTree [] stack = head stack
    buildTree (x:xs) stack
      | isUnaryOperator x = do
          let child = head stack
          let new_node = UnaryNode child x
          let updated_stack = tail stack
          buildTree xs (new_node : updated_stack)
      | isOperator x = do
          let right = head stack
          let left = stack !! 1
          let new_node = BinaryNode left right x
          let updated_stack = drop 2 stack
          buildTree xs (new_node : updated_stack)
      | otherwise = buildTree xs (Leaf x : stack)

isUnaryOperator :: Char -> Bool
isUnaryOperator x = x `elem` "?~*"

isOperator :: Char -> Bool
isOperator x = x `elem` ";>^v"

main :: IO ()
main = do 
    putStrLn "Enter the number of vertices:"
    vertices <- readLn
    matrix <- graph vertices
    putStrLn $ "Created matrix: " ++ show matrix
    set <- states vertices
    print set
    let postfixExpression = "a?ab;?"
    let root = buildExpressionTree postfixExpression
    putStrLn $ "Expression Tree: " ++ show root
