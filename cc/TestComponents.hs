module Main where

import qualified Data.Map.Strict as Map
import GraphComponents (findComponents)

import qualified Data.Map.Strict as Map

generateMultiComponent :: Int -> Int -> Map.Map Int [Int]
generateMultiComponent k size = Map.unions [Map.fromList [(i + j*size, [(i `mod` size) + 1 + j*size, if i == 1 then (j+1)*size else i - 1 + j*size]) | i <- [1..size]] | j <- [0..k-1]]

testGraph1 :: Map.Map Int [Int]
testGraph1 = Map.fromList [
    (1, [2, 3]),
    (2, [1, 3]),
    (3, [1, 2]),
    (4, [5]),
    (5, [4, 6]),
    (6, [5, 7]),
    (7, [6])
  ]

testGraph2 :: Map.Map Int [Int]
testGraph2 = Map.fromList [
    (1, [2]),
    (2, [1, 3]),
    (3, [2]),
    (4, [])
  ]

testGraph3 :: Map.Map Int [Int]
testGraph3 = Map.fromList [
    (1, [2, 3]),
    (2, [1, 3]),
    (3, [1, 2]),
    (4, [5]),
    (5, [4, 6]),
    (6, [5, 7]),
    (7, [6]),
    (8, []),
    (9, []),
    (10, [])
  ]

testGraph4 :: Map.Map Int [Int]
testGraph4 = Map.fromList []

largeMulti :: Map.Map Int [Int]
largeMulti = generateMultiComponent 100 1000

generateStarPaths :: Int -> Int -> Map.Map Int [Int]
generateStarPaths k l =
  let n = 1 + k * l
      centralEdges = [(1, i) | i <- [2, l+2 .. k*l+1]]
      pathEdges = [(i, i+1) | j <- [0..k-1], i <- [j*l+2 .. j*l+l]]
      allEdges = centralEdges ++ pathEdges ++ [(v, u) | (u, v) <- centralEdges ++ pathEdges]
      adjList = foldr (\(u, v) acc -> Map.insertWith (++) u [v] acc) Map.empty allEdges
      adjListWithAll = foldr (\i acc -> Map.insertWith (++) i [] acc) adjList [1..n]
  in adjListWithAll

largeStarPaths :: Map.Map Int [Int]
largeStarPaths = generateStarPaths 100 1000

main :: IO ()
main = do
  putStrLn "Test graph 1:"
  let components1 = findComponents testGraph1
  putStrLn $ "Connected components: " ++ show components1

  putStrLn "\nTest graph 2:"
  let components2 = findComponents testGraph2
  putStrLn $ "Connected components: " ++ show components2

  putStrLn "\nTest graph 3:"
  let components3 = findComponents testGraph3
  putStrLn $ "Connected components: " ++ show components3

  putStrLn "\nTest graph 4:"
  let components4 = findComponents testGraph4
  putStrLn $ "Connected components: " ++ show components4

  putStrLn "\nLarge multi-component graph (100 components, 1000 vertices each):"
  let components = findComponents largeMulti
  putStrLn $ "Connected components: " ++ show components

  putStrLn "\nLarge star-paths graph (100000 vertices):"
  let componentsStarPaths = findComponents largeStarPaths
  putStrLn $ "Connected components: " ++ show componentsStarPaths