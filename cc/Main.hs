module Main where

import qualified Data.Map.Strict as Map
import GraphComponents (findComponents)

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