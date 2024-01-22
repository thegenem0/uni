main :: IO ()
main = do
    -- gathering user input
  putStr "Enter graph type (directed/undirected): "
  -- implementation detail
  
  graph <- initGraph numNodes isDirected
  mainLoop graph isDirected

mainLoop :: Graph -> Bool -> IO ()
mainLoop graph isDirected = do
  putStrLn "1. Print graph"
  putStrLn "2. Add node"
  putStrLn "3. Remove node"
  putStrLn "4. Add edge"
  putStrLn "5. Remove edge"
  putStrLn "6. BFS"
  putStrLn "7. DFS"
  putStrLn "Enter option:"
  input <- getLine
  case readMaybe input :: Maybe Int of
    Just option -> do
      graph' <- processOption graph isDirected option
      mainLoop graph' isDirected
    Nothing -> do
      putStrLn "Invalid input, please enter a number."
      mainLoop graph isDirected