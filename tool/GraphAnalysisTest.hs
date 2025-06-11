module Main where

import GraphAnalysis
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort)
import Control.Monad (when, unless)
import Data.Bifunctor (first)

-- | Type for test result
data TestResult = Pass | Fail String deriving (Show)

-- | Run one test
runTest :: String -> Bool -> TestResult
runTest name True = Pass
runTest name False = Fail name

-- | Test cases for graph analysis functions
k3 = Map.fromList [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
isolated = Map.fromList [(1, [2]), (2, [1]), (3, [])]
mixed = Map.fromList [(1, [2, 3]), (2, [1]), (3, [1]), (4, [])]
disconnected = Map.fromList [(1, [2]), (2, [1]), (3, [4]), (4, [3]), (5, [])]
p3 = Map.fromList [(1, [2]), (2, [1, 3]), (3, [2])]
c4 = Map.fromList [(1, [2, 4]), (2, [1, 3]), (3, [2, 4]), (4, [1, 3])]
empty = Map.empty

-- | Tests for vertexDegrees
testVertexDegrees :: [TestResult]
testVertexDegrees =
  [ runTest "vertexDegrees: K3" $
      vertexDegrees k3 == Map.fromList [(1, 2), (2, 2), (3, 2)],
    runTest "vertexDegrees: isolated vertex" $
      vertexDegrees isolated == Map.fromList [(1, 1), (2, 1), (3, 0)]
  ]

-- | Tests for degreeStats
testDegreeStats :: [TestResult]
testDegreeStats =
  [ runTest "degreeStats: K3" $
      degreeStats k3 == (2, 2, Map.fromList [(2, 3)]),
    runTest "degreeStats: different degrees" $
      degreeStats mixed == (2, 0, Map.fromList [(0, 1), (1, 2), (2, 1)])
  ]

-- | Tests for findComponents
testFindComponents :: [TestResult]
testFindComponents =
  [ runTest "findComponents: K3" $
      sort (map sort (findComponents k3)) == [[1, 2, 3]],
    runTest "findComponents: disconnected" $
      sort (map sort (findComponents disconnected)) == [[1, 2], [3, 4], [5]]
  ]

-- | Tests for componentDiameters
testComponentDiameters :: [TestResult]
testComponentDiameters =
  [ runTest "componentDiameters: K3" $
      sort (map (first sort) (componentDiameters k3)) == [([1, 2, 3], 1)],
    runTest "componentDiameters: P3" $
      sort (map (first sort) (componentDiameters p3)) == [([1, 2, 3], 2)],
    runTest "componentDiameters: disconnected" $
      sort (map (first sort) (componentDiameters disconnected)) == [([1, 2], 1), ([3, 4], 1), ([5], 0)]
  ]

-- | Tests for distanceStats
testDistanceStats :: [TestResult]
testDistanceStats =
  [ runTest "distanceStats: K3" $
      sort (map (first sort) (distanceStats k3)) == [([1, 2, 3], Map.fromList [(1, 6)])],
    runTest "distanceStats: P3" $
      sort (map (first sort) (distanceStats p3)) == [([1, 2, 3], Map.fromList [(1, 4), (2, 2)])],
    runTest "distanceStats: isolated vertex" $
      sort (map (first sort) (distanceStats isolated)) == [([1, 2], Map.fromList [(1, 2)]), ([3], Map.empty)]
  ]

-- | Tests for clusteringCoefficient
testClusteringCoefficient :: [TestResult]
testClusteringCoefficient =
  [ runTest "clusteringCoefficient: K3 vertex 1" $
      abs (clusteringCoefficient k3 1 - 1.0) < 1e-10,
    runTest "clusteringCoefficient: P3 vertex 2" $
      abs (clusteringCoefficient p3 2 - 0.0) < 1e-10,
    runTest "clusteringCoefficient: isolated vertex 3" $
      abs (clusteringCoefficient isolated 3 - 0.0) < 1e-10,
    runTest "clusteringCoefficient: cycle C4 vertex 1" $
      abs (clusteringCoefficient c4 1 - 0.0) < 1e-10
  ]

-- | Tests for globalClusteringCoefficient
testGlobalClusteringCoefficient :: [TestResult]
testGlobalClusteringCoefficient =
  [ runTest "globalClusteringCoefficient: K3" $
      abs (globalClusteringCoefficient k3 - 1.0) < 1e-10,
    runTest "globalClusteringCoefficient: P3" $
      abs (globalClusteringCoefficient p3 - 0.0) < 1e-10,
    runTest "globalClusteringCoefficient: empty graph" $
      abs (globalClusteringCoefficient empty - 0.0) < 1e-10,
    runTest "globalClusteringCoefficient: cycle C4" $
      abs (globalClusteringCoefficient c4 - 0.0) < 1e-10
  ]

-- | Run tests and show results
main :: IO ()
main = do
  let allTests = concat
        [ testVertexDegrees
        , testDegreeStats
        , testFindComponents
        , testComponentDiameters
        , testDistanceStats
        , testClusteringCoefficient
        , testGlobalClusteringCoefficient
        ]
      passed = length [ () | Pass <- allTests ]
      failed = [ name | Fail name <- allTests ]
  putStrLn $ "Tests passed: " ++ show passed ++ "/" ++ show (length allTests)
  unless (null failed) $ do
    putStrLn "Failures:"
    mapM_ (\name -> putStrLn $ "  - " ++ name) failed