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

-- | Tests for vertexDegrees
testVertexDegrees :: [TestResult]
testVertexDegrees =
  [ runTest "vertexDegrees: K3" $
      vertexDegrees k3 == Map.fromList [(1, 2), (2, 2), (3, 2)],
    runTest "vertexDegrees: isolated vertex" $
      vertexDegrees isolated == Map.fromList [(1, 1), (2, 1), (3, 0)]
  ]
  where
    k3 = Map.fromList [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
    isolated = Map.fromList [(1, [2]), (2, [1]), (3, [])]

-- | Tests for degreeStats
testDegreeStats :: [TestResult]
testDegreeStats =
  [ runTest "degreeStats: K3" $
      degreeStats k3 == (2, 2, Map.fromList [(2, 3)]),
    runTest "degreeStats: different degrees" $
      degreeStats mixed == (2, 0, Map.fromList [(0, 1), (1, 2), (2, 1)])
  ]
  where
    k3 = Map.fromList [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
    mixed = Map.fromList [(1, [2, 3]), (2, [1]), (3, [1]), (4, [])]

-- | Tests for findComponents
testFindComponents :: [TestResult]
testFindComponents =
  [ runTest "findComponents: K3" $
      sort (map sort (findComponents k3)) == [[1, 2, 3]],
    runTest "findComponents: disconnected" $
      sort (map sort (findComponents disconnected)) == [[1, 2], [3, 4], [5]]
  ]
  where
    k3 = Map.fromList [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
    disconnected = Map.fromList [(1, [2]), (2, [1]), (3, [4]), (4, [3]), (5, [])]

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
  where
    k3 = Map.fromList [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
    p3 = Map.fromList [(1, [2]), (2, [1, 3]), (3, [2])]
    disconnected = Map.fromList [(1, [2]), (2, [1]), (3, [4]), (4, [3]), (5, [])]

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
  where
    k3 = Map.fromList [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
    p3 = Map.fromList [(1, [2]), (2, [1, 3]), (3, [2])]
    isolated = Map.fromList [(1, [2]), (2, [1]), (3, [])]

-- | Run tests and show results
main :: IO ()
main = do
  let allTests = concat
        [ testVertexDegrees
        , testDegreeStats
        , testFindComponents
        , testComponentDiameters
        , testDistanceStats
        ]
      passed = length [ () | Pass <- allTests ]
      failed = [ name | Fail name <- allTests ]
  putStrLn $ "Tests passed: " ++ show passed ++ "/" ++ show (length allTests)
  unless (null failed) $ do
    putStrLn "Failures:"
    mapM_ (\name -> putStrLn $ "  - " ++ name) failed