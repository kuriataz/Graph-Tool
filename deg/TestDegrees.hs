module Main where

import qualified Data.Map.Strict as Map
import GraphDegrees (vertexDegrees, degreeStats)

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
    (1, [2, 3, 4, 5, 6]),
    (2, [1]),
    (3, [1]),
    (4, [1]),
    (5, [1]),
    (6, [1])
  ]

testGraph4 :: Map.Map Int [Int]
testGraph4 = Map.fromList []

main :: IO ()
main = do
  putStrLn "TestGraph 1:"
  let degrees1 = vertexDegrees testGraph1
      (maxDeg1, minDeg1, hist1) = degreeStats testGraph1
  putStrLn $ "Degrees: " ++ show degrees1
  putStrLn $ "Max: " ++ show maxDeg1
  putStrLn $ "Min: " ++ show minDeg1
  putStrLn $ "Histogram: " ++ show hist1

  putStrLn "\nTest Graph 2:"
  let degrees2 = vertexDegrees testGraph2
      (maxDeg2, minDeg2, hist2) = degreeStats testGraph2
  putStrLn $ "Graph: " ++ show testGraph2
  putStrLn $ "Degrees: " ++ show degrees2
  putStrLn $ "Max: " ++ show maxDeg2
  putStrLn $ "Min: " ++ show minDeg2
  putStrLn $ "Histogram: " ++ show hist2

  putStrLn "\nTest Graph 3:"
  let degrees3 = vertexDegrees testGraph3
      (maxDeg3, minDeg3, hist3) = degreeStats testGraph3
  putStrLn $ "Graph: " ++ show testGraph3
  putStrLn $ "Degrees: " ++ show degrees3
  putStrLn $ "Max: " ++ show maxDeg3
  putStrLn $ "Min: " ++ show minDeg3
  putStrLn $ "Histogram: " ++ show hist3

  putStrLn "\nTest Graph 4:"
  let degrees4 = vertexDegrees testGraph4
      (maxDeg4, minDeg4, hist4) = degreeStats testGraph4
  putStrLn $ "Graph: " ++ show testGraph4
  putStrLn $ "Degrees: " ++ show degrees4
  putStrLn $ "Max: " ++ show maxDeg4
  putStrLn $ "Min: " ++ show minDeg4
  putStrLn $ "Histogram: " ++ show hist4


