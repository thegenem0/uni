{-# LANGUAGE ImportQualifiedPost #-}

-- GraheER - Haskell implementation of a graph editor

import Control.Monad (replicateM)
import Data.Char
import Data.Graph (graphFromEdges)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import System.IO
import System.Random
import Text.Read (Lexeme (String), readEither, readMaybe)

type Graph = Map.Map Int [Int]

generateEdges :: Int -> Int -> StdGen -> ([Int], StdGen)
generateEdges numNodes currentNode generate = foldr f ([], generate) [0 .. numNodes - 1]
  where
    f node (edges, g) =
      if node /= currentNode
        then
          let (randNum, newGen) = randomR (0, 1) g
           in if randNum == (1 :: Int)
                then (node : edges, newGen)
                else (edges, newGen)
        else (edges, g)

initGraph :: Int -> Bool -> IO Graph
initGraph numNodes isDirected = do
  gen <- newStdGen
  let nodes = [0 .. numNodes - 1]
  edgesList <- mapM (`generateNodeEdges` gen) nodes
  return $ Map.fromList edgesList
  where
    generateNodeEdges node gen = do
      let (edges, newGen) = generateEdges numNodes node gen
      return (node, edges)

printGraph :: Graph -> IO ()
printGraph graph = do
  putStrLn "Graph:"
  mapM_ printNode $ Map.toList graph
  where
    printNode (node, edges) = do
      putStr $ "Node " ++ show node ++ ": "
      print edges

addNode :: Int -> Graph -> Graph
addNode node graph
  | Map.member node graph = graph
  | otherwise = Map.insert node [] graph

removeNode :: Int -> Graph -> Graph
removeNode node graph = Map.map removeEdge updatedGraph
  where
    updatedGraph = Map.delete node graph
    removeEdge = filter (/= node)

addEdge :: Int -> Int -> Bool -> Graph -> Graph
addEdge src dst isDirected graph = graph''
  where
    graph' = Map.adjust (dst :) src graph
    graph'' = if isDirected then graph' else Map.adjust (src :) dst graph'

removeEdge :: Int -> Int -> Bool -> Graph -> Graph
removeEdge src dst isDirected graph = graph''
  where
    removeDst = filter (/= dst)
    graph' = Map.adjust removeDst src graph
    graph'' = if isDirected then graph' else Map.adjust (filter (/= src)) dst graph'

bfs :: Graph -> Int -> [Int]
bfs graph start = bfs' [start] Set.empty
  where
    bfs' [] _ = []
    bfs' (x : xs) visited
      | Set.member x visited = bfs' xs visited
      | otherwise = x : bfs' (xs ++ nextNodes) (Set.insert x visited)
      where
        nextNodes = fromMaybe [] (Map.lookup x graph)

dfs :: Graph -> Int -> [Int]
dfs graph start = dfs' [start] Set.empty
  where
    dfs' [] _ = []
    dfs' (x : xs) visited
      | Set.member x visited = dfs' xs visited
      | otherwise = x : dfs' (nextNodes ++ xs) (Set.insert x visited)
      where
        nextNodes = fromMaybe [] $ Map.lookup x graph

main :: IO ()
main = do
  putStr "Enter graph type (directed/undirected): "
  hFlush stdout
  input <- getLine
  let inputLower = map toLower input
  let isDirected = case inputLower of
        "directed" -> True
        "undirected" -> False
        _ -> error "Invalid input, exiting..."
  putStrLn $ "Graph type selected: " ++ if isDirected then "Directed" else "Undirected"

  putStr "How many nodes should the graph have? "
  hFlush stdout
  input <- getLine
  let numNodes = readEither input :: Either String Int
  numNodes <- case numNodes of
    Left _ -> do
      putStrLn "Invalid input, exiting..."
      return 0
    Right numNodes -> return numNodes
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

processOption :: Graph -> Bool -> Int -> IO Graph
processOption graph isDirected option = case option of
  1 -> do
    printGraph graph
    return graph
  2 -> do
    let graph' = addNode (Map.size graph) graph
    putStrLn $ "Added node " ++ show (Map.size graph)
    return graph'
  3 -> do
    putStrLn "Enter node to remove:"
    nodeInput <- getLine
    case readMaybe nodeInput of
      Just node -> do
        putStrLn $ "Removing node " ++ show node
        let graph' = removeNode node graph
        printGraph graph'
        return graph'
      Nothing ->
        putStrLn "Invalid input, please enter a number."
          >> return graph
  4 -> do
    putStrLn "Enter source node:"
    srcInput <- getLine
    putStrLn "Enter destination node:"
    dstInput <- getLine
    case (readMaybe srcInput, readMaybe dstInput) of
      (Just src, Just dst) -> do
        putStrLn $ "Adding edge from " ++ show src ++ " to " ++ show dst
        let graph' = addEdge src dst isDirected graph
        printGraph graph'
        return graph'
      _fail -> putStrLn "Invalid input, please enter a number." >> return graph
  5 -> do
    putStrLn "Enter source node:"
    srcInput <- getLine
    putStrLn "Enter destination node:"
    dstInput <- getLine
    case (readMaybe srcInput, readMaybe dstInput) of
      (Just src, Just dst) -> do
        putStrLn $ "Removing edge from " ++ show src ++ " to " ++ show dst
        let graph' = removeEdge src dst isDirected graph
        printGraph graph'
        return graph'
      _fail -> putStrLn "Invalid input, please enter a number." >> return graph
  6 -> do
    putStrLn "Enter start node:"
    nodeInput <- getLine
    case readMaybe nodeInput of
      Just node -> do
        putStrLn $ "Running BFS from node " ++ show node
        print $ bfs graph node
        return graph
      Nothing -> putStrLn "Invalid input, please enter a number." >> return graph
  7 -> do
    putStrLn "Enter start node:"
    nodeInput <- getLine
    case readMaybe nodeInput of
      Just node -> do
        putStrLn $ "Running DFS from node " ++ show node
        print $ dfs graph node
        return graph
      Nothing -> putStrLn "Invalid input, please enter a number." >> return graph
  _ -> putStrLn "Invalid option" >> return graph
